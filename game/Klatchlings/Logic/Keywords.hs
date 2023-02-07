module Logic.Keywords where

import qualified Data.Map as Map (fromList)

import Base.Ability
import Base.Fields
import Base.Card
import Base.Resolves
import Base.GameState

import Logic.Battle

dummyUnit :: Int -> Owner -> Card
dummyUnit posn o = mint
          . fst . equip battleUnit
          . fst . equip resolveUnit
          . fst . set NominateFlag 0
          . fst . modify Fatigued (strictSet 0)
          . fst . modify Power (strictSet 1)
          . fst . modify Toughness (strictSet 1)
          . fst . modify Cost (strictSet 1)
          . fst . set Owner (fromEnum o)
          . fst . set Position posn
          . fst . set Zone (fromEnum MidDeck)
          $ blank

dummyHero :: Owner -> Card
dummyHero o = mint
            . fst . modify Toughness (strictSet 5)
            . fst . set Mana 3
            . fst . set Owner (fromEnum o)
            . fst . set Zone (fromEnum Throne)
            $ blank

resolveUnit :: Ability
resolveUnit = Ability System OnTrigger trg grd trgts rslvs
  where
    trg = selfEntered Stack
    grd = oneOf (inZone Throne) $ inZone Stack

    trgts1 = convert [0] targetHero
    trgts2 = convert [1] targetSelf
    trgts = trgts1 <> trgts2

    rslvs = Map.fromList
            [ (TID 0, payCost)
            , (TID 1, orToHand $ moveZone Barrack)
            ]

battleUnit :: Ability
battleUnit = Ability System OnTrigger trg grd trgts rslvs
  where
    trg = both (whenInZone Battlefield) $ enteredPhase Skirmish
    grd = alwaysOk

    trgts = convert (repeat 0) aimStrikes

    rslvs = Map.fromList [(TID 0, doStrike)]
