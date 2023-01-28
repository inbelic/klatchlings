module Internal.Display
  ( encodeDiff
  ) where

import Internal.Types
import Base.Fields
import Base.GameState

import qualified Data.Map as Map
  ( Map, empty
  , foldrWithKey, filterWithKey
  , lookup, insert
  )

encodeDiff :: CardState -> GameState -> String
encodeDiff old (GameState _ _ new)
  = encode stdSchema . compDiff old $ new

newtype Schema = Schema
  { schema :: [(Zone, [Field])]
  }

stdSchema :: Schema
stdSchema = Schema
  [ (Hand, [Attr Owner, Stat Cost, Stat Power, Stat Toughness])
  , (Stack, [Attr Owner, Attr Position])
  , (Barrack, [Attr Owner, Stat Power, Stat Toughness])
  , (Grave, [Attr Owner, Attr Position])
  , (Battlefield, [Attr Owner, Stat Power, Stat Toughness, Stat Fatigued])
  , (Throne, [Attr Owner, Attr Mana])
  , (TopDeck, [Attr Owner, Attr Position])
  , (MidDeck, [Attr Owner])
  , (BotDeck, [Attr Owner, Attr Position])
  ]

encode :: Schema -> CardState -> String
encode (Schema flts) cs = foldr (f cs) "" flts
  where
    f cs (z, flds) acc
        = (++) (show z ++ ": [")
        . Map.foldrWithKey (g flds) ("], " ++ acc)
        . refine (Attr Zone) ((==) (fromEnum z))
        $ cs

    g flds cID fm acc = (encodeCard flds cID fm) ++ acc

encodeCard :: [Field] -> CardID -> FieldMap -> String
encodeCard toInclude cID fm
  = frnt ++ Map.foldrWithKey f "} " fm'
  where fm' = Map.filterWithKey (\fld _ -> elem fld toInclude) fm
        frnt = show cID ++ " {"

        f fld x acc = show fld ++ " => " ++ show x ++ ", " ++ acc

compDiff :: CardState -> CardState -> CardState
compDiff old = Map.foldrWithKey (compDiff' old) Map.empty
  where
    compDiff' :: CardState -> CardID -> FieldMap -> CardState -> CardState
    compDiff' old cID fm new
      = case Map.lookup cID old of
          Nothing -> Map.insert cID fm new
          (Just oldFM) ->
            let fm' = Map.filterWithKey (takeAltered oldFM) fm
             in Map.insert cID fm' new

    takeAltered :: FieldMap -> Field -> Int -> Bool
    takeAltered oldFM fld x
      = case Map.lookup fld oldFM of
          Nothing -> True
          (Just oldX) -> oldX /= x
