module Internal.Card where

import Internal.Fields
import Internal.Types

import qualified Data.Map as Map
  ( empty, adjust
  , insert, lookup
  , keys
  )

data Card = Card
  { attrs       :: AttrMap
  , stats       :: StatMap
  , abilities   :: AbilityMap
  , original    :: Maybe Card
  }

blank :: Card
blank = Card Map.empty Map.empty Map.empty Nothing

mint :: Card -> Card
mint (Card ats sts abs _) = crd . Just . crd $ Nothing
  where crd = Card ats sts abs

reload :: Card -> Card
reload crd@(Card _ _ _ Nothing) = crd
reload (Card _ _ _ (Just oc)) = Card ats sts abs (Just oc)
  where (Card ats sts abs _) = oc

newtype Change = Change
  { change :: Card -> (Card, Alteration)
  }

data Alteration
  = Shift Field Int     -- We shifted the Field value by Int
  | Set Attr Int        -- We set an Attr to a value Int
  | Replace Stat        -- We modified the Status of a Stat
  | Equip               -- We gave the card an ability

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
  let abs' = Map.insert aID ablty abs
      aID = AbilityID . (+ 1) . maximum' . map abltyID . Map.keys $ abs
   in (Card ats sts abs' oc,  Equip)

maximum' :: [Int] -> Int
maximum' [] = 0
maximum' xs = maximum xs
