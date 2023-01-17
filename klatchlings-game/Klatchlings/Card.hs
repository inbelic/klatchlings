module Card
  ( Card
  , Cards
  , Change(..)
  , Alteration(..)
  , blank
  , mint
  , reload
  , shift
  , set
  , replace
  , equip
  -- Internal Usage
  , view
  , headers
  , collectTriggered
  , targetChanges
  , applyChange
  ) where

import Internal.Fields
import Internal.Types

import qualified Data.Map as Map
  ( Map, empty, adjust
  , insert, lookup
  , keys
  , foldrWithKey
  , union, map
  )

blank :: Card
blank = Card Map.empty Map.empty [] Nothing

mint :: Card -> Card
mint (Card ats sts abs _) = crd . Just . crd $ Nothing
  where crd = Card ats sts abs

reload :: Card -> Card
reload crd@(Card _ _ _ Nothing) = crd
reload (Card _ _ _ (Just oc)) = Card ats sts abs (Just oc)
  where (Card ats sts abs _) = oc

-- Note: if the field is not set then it will silently not do anything.
-- FIXME: is this an issue?
shift :: Field -> Int -> Change
shift (Attr atr) x = Change $ shiftAttr atr x
shift (Stat stt) x = Change $ shiftStat stt x

shiftAttr :: Attr -> Int -> Card -> (Card, Alteration)
shiftAttr atr x (Card ats sts abs oc)
  = (Card ats' sts abs oc, Shift (Attr atr) x)
    where ats' = Map.adjust (+ x) atr ats

shiftStat :: Stat -> Int -> Card -> (Card, Alteration)
shiftStat stt x (Card ats sts abs oc)
  = (Card ats sts' abs oc, Shift (Stat stt) x)
    where sts' = Map.adjust (f <>) stt sts
          f = Status $ const (+ x)

set :: Attr -> Int -> Change
set atr x = Change $ \(Card ats sts abs oc) ->
  let ats' = Map.insert atr x ats
   in (Card ats' sts abs oc, Set atr x)

replace :: Stat -> Status -> Change
replace stt f = Change $ \(Card ats sts abs oc) ->
  let sts' = case Map.lookup stt sts of
               Nothing -> Map.insert stt f sts
               (Just g) -> Map.insert stt (f <> g) sts
   in (Card ats sts' abs oc, Replace stt)

equip :: Ability -> Change
equip ablty = Change $ \(Card ats sts abs oc) ->
  let abs' = ablty : abs
   in (Card ats sts abs' oc,  Equip)

maximum' :: [Int] -> Int
maximum' [] = 0
maximum' xs = maximum xs


-- Functions for internal use
view :: Cards -> CardState
view crds = Map.union (Map.map attrToField as) (Map.map statToField ss)
  where as = viewAttrState crds
        ss = viewStatState as crds

viewAttrState :: Cards -> AttrState
viewAttrState = Map.map attrs

viewStatState :: AttrState -> Cards -> StatState
viewStatState as = Map.map (Map.map (apply . status) . stats)
  where apply f = f as 0

attrToField :: AttrMap -> FieldMap
attrToField = Map.foldrWithKey (\k v m -> Map.insert (Attr k) v m) Map.empty

statToField :: StatMap' -> FieldMap
statToField = Map.foldrWithKey (\k v m -> Map.insert (Stat k) v m) Map.empty

headers :: GameState -> Cards -> [Header]
headers gs = Map.foldrWithKey f []
  where f cID crd = (++) (collectTriggered gs cID crd)

collectTriggered :: GameState -> CardID -> Card -> [Header]
collectTriggered gs cID
  = foldr (toHeader gs cID) [] . filter (triggered gs cID) . abilities
    where
      triggered :: GameState -> CardID -> Ability -> Bool
      triggered gs cID (Ability _ trg _ _ _) = trigger trg cID gs

      toHeader :: GameState -> CardID -> Ability -> [Header] -> [Header]
      toHeader gs cID (Ability tmg trg grd trgts chngs)
        | tmg == OnResolve = (:) (Unassigned cID grd trgts chngs)
        | tmg == OnTrigger = (:) (Assigned cID grd
                                  $ targetChanges gs cID chngs trgts)

targetChanges :: GameState -> CardID -> Changes -> Targets -> [(Change, Target)]
targetChanges gs cID chngs trgts = foldr getChange [] $ getTargets trgts cID gs
  where
    getChange (tID, trgt) acc
      = case Map.lookup tID chngs of
          Nothing -> acc
          (Just chng) -> (:) (chng, trgt) acc

applyChange :: Change -> CardID -> Cards -> Maybe (Alteration, Cards)
applyChange chng tcID crds
  = case Map.lookup tcID crds of
      Nothing -> Nothing
      (Just crd) ->
        let (crd', alt) = change chng crd
         in Just (alt, Map.insert tcID crd' crds)
