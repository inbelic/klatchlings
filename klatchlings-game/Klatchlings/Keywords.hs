module Keywords where

import qualified Data.Map as Map
  ( Map, empty, fromList, keys
  )

import Ability
import Fields
import Card
import Resolves
import GameState

resolveUnit :: Ability
resolveUnit = Ability System OnTrigger trg grd trgts rslvs
  where
    trg = selfEntered Stack
    grd = inZone Stack

    trgts = convert (repeat 0) targetSelf
    rslvs = Map.fromList
            [(TID 0, independent $ set Zone (fromEnum Barrack))]
