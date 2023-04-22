module Base.GameState
  ( GameState(..)
  , CardState(..)
  , CardID(..)
  , peek
  , hdrOwner
  , retreive
  , refine
  , within
  , orderBy
  , getZone
  , getOwnersZone
  , getHero
  , getActive
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

hdrOwner :: GameState -> Header -> Owner
hdrOwner gs hdr
  = toEnum . retreive (getHdrCID hdr) (Attr Owner) . getCS $ gs

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
          | cID `elem` cIDs = (:) (cID, x)
          | otherwise = id

getZone :: Zone -> CardState -> [CardID]
getZone z = within
          . refine (Attr Zone) (fromEnum z ==)

getOwnersZone :: Owner -> Zone -> CardState -> [CardID]
getOwnersZone o z
  = within
  . refine (Attr Owner) (fromEnum o ==)
  . refine (Attr Zone) (fromEnum z ==)

getHero :: Owner -> CardState -> CardID
getHero o = head . getOwnersZone o Throne

getActive :: Phase -> CardState -> Owner
getActive p cs
  = case (p, retreive (CardID 0) (Attr AttackFlag) cs) of
      (Seige, 0) -> P1
      (Seige, 1) -> P2
      (Retaliate, 0) -> P2
      (Retaliate, 1) -> P1
      _ -> undefined
