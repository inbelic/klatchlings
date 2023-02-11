module Internal.Display
  ( displayState
  ) where

import Internal.Types
import Base.Fields
import Base.GameState
import Control.Concurrent.Chan (Chan, readChan, writeChan)

import qualified Data.Map as Map
  ( Map, empty
  , foldrWithKey, filterWithKey
  , lookup, insert
  , map, keys
  )

displayState :: CardState -> Chan String -> GameState -> IO ()
displayState prevCS ch gs = do
  writeChan ch . (++) "view: " . encodeDiff prevCS $ gs
  _ <- readChan ch
  return ()

encodeDiff :: CardState -> GameState -> String
encodeDiff old (GameState _ _ new)
  = encode allySchema p1Diff ++ encode enemySchema p2Diff
  ++ "~"
  ++ encode allySchema p2Diff ++ encode enemySchema p1Diff
    where
      diff = compDiff old new
      p1Diff = refine (Attr Owner) ((==) (fromEnum P1)) diff
      p2Diff = refine (Attr Owner) ((==) (fromEnum P2)) diff

newtype Schema = Schema
  { schema :: [(Zone, [Field])]
  }

allySchema :: Schema
allySchema = Schema
  [ (Hand, [Attr Owner, Stat Cost, Stat Power, Stat Toughness])
  , (Stack, [Attr Owner, Attr Position])
  , (Barrack, [Attr Owner, Stat Power, Stat Toughness])
  , (Grave, [Attr Owner, Attr Position])
  , (Battlefield, [Attr Owner, Stat Power, Stat Toughness, Stat Fatigued])
  , (Throne, [Attr Owner, Attr Mana, Stat Toughness])
  , (TopDeck, [Attr Owner, Attr Position])
  , (MidDeck, [Attr Owner])
  , (BotDeck, [Attr Owner, Attr Position])
  ]

enemySchema :: Schema
enemySchema = Schema
  [ (Hand, [Attr Owner])
  , (Stack, [Attr Owner, Attr Position])
  , (Barrack, [Attr Owner, Stat Power, Stat Toughness])
  , (Grave, [Attr Owner, Attr Position])
  , (Battlefield, [Attr Owner, Stat Power, Stat Toughness, Stat Fatigued])
  , (Throne, [Attr Owner, Attr Mana, Stat Toughness])
  , (TopDeck, [Attr Owner])
  , (MidDeck, [Attr Owner])
  , (BotDeck, [Attr Owner])
  ]

encode :: Schema -> CardState -> String
encode (Schema flts) cs = foldr (f cs) "" flts
  where
    f cs (z, flds) acc
        = case Map.foldrWithKey (g flds) ""
              . refine (Attr Zone) ((==) (fromEnum z)) $ cs
          of
            [] -> acc
            valStr ->
              show (fromEnum z) ++ ": [" ++ valStr ++ "], " ++ acc

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
      = case (Map.lookup cID old, Map.lookup (Attr Zone) fm) of
          (_, Nothing) -> new
          (Nothing, _) -> Map.insert cID fm new
          (Just oldFM, curZone) ->
            case (==) curZone . Map.lookup (Attr Zone) $ oldFM of
              False -> Map.insert cID fm new
              True ->
                let fm' = Map.filterWithKey (takeAltered oldFM) fm
                 in Map.insert cID fm' new

    takeAltered :: FieldMap -> Field -> Int -> Bool
    takeAltered oldFM fld x
      = case Map.lookup fld oldFM of
          Nothing -> True
          (Just oldX) -> oldX /= x
