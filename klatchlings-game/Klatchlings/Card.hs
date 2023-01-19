module Card
  ( Card
  , Cards
  , Change
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
  , targetResolves
  , applyResolve
  ) where

import Fields (Field(..), Stat, Attr)
import Internal.Types
import Internal.Misc (getNextKey)

import qualified Data.Map as Map
  ( Map, empty, adjust
  , insert, lookup
  , keys, toList
  , foldrWithKey
  , union, map
  )

blank :: Card
blank = Card Map.empty Map.empty Map.empty Nothing

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
shift (Attr atr) x = shiftAttr atr x
shift (Stat stt) x = shiftStat stt x

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
set atr x (Card ats sts abs oc) = (Card ats' sts abs oc, Set atr x)
  where ats' = Map.insert atr x ats

replace :: Stat -> Status -> Change
replace stt f (Card ats sts abs oc) = (Card ats sts' abs oc, Replace stt)
  where sts' = case Map.lookup stt sts of
               Nothing -> Map.insert stt f sts
               (Just g) -> Map.insert stt (f <> g) sts

equip :: Ability -> Change
equip ablty (Card ats sts abs oc) = (Card ats sts abs' oc,  Equip)
  where abs' = Map.insert aID ablty abs
        aID = AbilityID . getNextKey abilityID $ abs

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
  = foldr (toHeader gs cID) [] . filter (triggered gs cID)
  . Map.toList . abilities
    where
      triggered :: GameState -> CardID -> (AbilityID, Ability) -> Bool
      triggered gs cID (_, Ability _ trg _ _ _) = trigger trg cID gs

      toHeader :: GameState -> CardID -> (AbilityID, Ability) -> [Header] -> [Header]
      toHeader gs cID (aID, Ability tmg trg grd trgts rslvs)
        | tmg == OnResolve = (:) (Unassigned cID aID grd trgts rslvs)
        | tmg == OnTrigger = (:) (Assigned cID aID grd
                                  $ targetResolves gs cID rslvs trgts)

targetResolves :: GameState -> CardID -> Resolves -> Targets -> [(Resolve, Target)]
targetResolves gs cID rslvs trgts = foldr getResolve [] $ getTargets trgts cID gs
  where
    getResolve (tID, trgt) acc
      = case Map.lookup tID rslvs of
          Nothing -> acc
          (Just rslv) -> (:) (rslv, trgt) acc

applyResolve :: Resolve -> CardID -> CardID -> GameState -> Cards -> (Alteration, Cards)
applyResolve rslv cID tcID gs crds
  = case Map.lookup tcID crds of
      Nothing ->
        let (crd', alt) = resolve rslv cID tcID gs blank
         in (alt, Map.insert tcID crd' crds)
      (Just crd) ->
        let (crd', alt) = resolve rslv cID tcID gs crd
         in (alt, Map.insert tcID crd' crds)
