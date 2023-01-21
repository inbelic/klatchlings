module Logic.Keywords where

import qualified Data.Map as Map (fromList)

import Base.Ability
import Base.Fields
import Base.Card
import Base.Resolves
import Base.GameState

dummyUnit :: Int -> Owner -> Card
dummyUnit posn o = mint
          . fst . equip resolveUnit
          . fst . set NominateFlag 0
          . fst . modify Fatigued (strictSet 0)
          . fst . set Owner (fromEnum o)
          . fst . set Position posn
          . fst . set Zone (fromEnum MidDeck)
          $ blank

resolveUnit :: Ability
resolveUnit = Ability System OnTrigger trg grd trgts rslvs
  where
    trg = selfEntered Stack
    grd = inZone Stack

    trgts = convert (repeat 0) targetSelf
    rslvs = Map.fromList
            [(TID 0, independent $ set Zone (fromEnum Barrack))]

battleUnit :: Ability
battleUnit = Ability System OnTrigger trg grd trgts rslvs
  where
    trg = enteredPhase Skirmish
    grd = alwaysOk

    trgts = undefined

    rslvs = undefined
