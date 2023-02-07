module Base.GameState
  ( GameState(..)
  , CardState(..)
  , CardID(..)
  , peek
  , retreive
  , refine
  , within
  , orderBy
  , getZone
  , getOwnersZone
  , getHero
  ) where

import Base.Card (view)
import Base.Fields
import Internal.Types
import Data.List (sortOn)

import qualified Data.Map as Map
  ( Map, empty, adjust
  , insert, lookup
  , keys, toList
  , foldrWithKey
  , union, map
  , filter
  )


peek :: Game -> GameState
peek (Game stck hist crds)
  = GameState stck hist . view $ crds

-- Will default to 0
retreive :: CardID -> Field -> CardState -> Int
retreive cID fld cs
  = case Map.lookup fld =<< Map.lookup cID cs of
      Nothing -> 0
      (Just x) -> x

refine :: Field -> (Int -> Bool) -> CardState -> CardState
refine fld cond = Map.filter cond' 
  where cond' fm = case Map.lookup fld fm of
                     Nothing -> False
                     (Just x) -> cond x

within :: CardState -> [CardID]
within = Map.keys

orderBy :: Field -> [CardID] -> CardState -> [(CardID, Int)]
orderBy fld cIDs
  = sortOn snd . Map.foldrWithKey f [] . Map.map (Map.lookup fld)
  where f _ Nothing = id
        f cID (Just x)
          | elem cID cIDs = (:) (cID, x)
          | otherwise = id

getZone :: Zone -> CardState -> [CardID]
getZone z = within
          . refine (Attr Zone) ((==) (fromEnum z))

getOwnersZone :: Owner -> Zone -> CardState -> [CardID]
getOwnersZone o z
  = within
  . refine (Attr Owner) ((==) (fromEnum o))
  . refine (Attr Zone) ((==) (fromEnum z))

getHero :: Owner -> CardState -> CardID
getHero o = head . getOwnersZone o Throne
