module Rules where

import qualified Data.Map as Map
  ( Map, empty, fromList, keys
  )

import Ability
import Fields
import Card
import Resolves
import GameState
import Status

import Keywords

cards = Map.fromList
  [ rules
  , (CardID 1, dummyUnit P1)
  , (CardID 2, dummyUnit P2)
  ]

dummyUnit :: Owner -> Card
dummyUnit owner
  = mint
  . fst . equip resolveUnit
  . fst . modify Fatigued (strictSet 0)
  . fst . set NominateFlag 0
  . fst . set Owner (fromEnum owner)
  . fst . set Zone (fromEnum MidDeck)
  $ blank

rules :: (CardID, Card)
rules = (CardID 0, crd)
  where crd = mint
            . fst . equip setActive
            . fst . equip nightPhase
            . fst . equip retreatPhase
            . fst . equip formationPhase
            . fst . equip nominatePhase
            . fst . (equip $ play Retaliate)
            . fst . (equip $ play Seige)
            . fst . equip morningPhase
            . fst . equip phaseControl
            . fst . set Phase (fromEnum Start)
            . fst . set ActiveFlag 0
            . fst . set AttackFlag 0
            $ blank


phaseControl :: Ability
phaseControl = Ability System OnTrigger trg grd trgts rslvs
  where
    trg = both clearStack clearCurrent
    grd = emptyStack

    trgts = targetRuleCard . Map.keys $ rslvs
    rslvs = Map.fromList [(TID 0, advancePhase)]

morningPhase :: Ability
morningPhase = Ability System OnResolve trg grd trgts rslvs
  where
    trg = enteredPhase Morning
    grd = oneOf (inZone TopDeck)
        . oneOf (inZone MidDeck)
        $ inZone BotDeck

    trgts = convert [0, 0] . combine (toDraw P1) $ (toDraw P2)
    rslvs = Map.fromList [(TID 0, independent $ set Zone (fromEnum Hand))]

play :: Phase -> Ability
play p = Ability Player OnResolve trg grd trgts rslvs
  where
    trg = both clearStack . both clearCurrent . both activeSet $ inPhase p 
    grd = oneOf isRulesCard $ inZone Hand

    trgts = convert [0] (validPlays p)
    rslvs = Map.fromList
            [(TID 0, orSetUnactive . independent $ set Zone (fromEnum Stack))]

setActive :: Ability
setActive = Ability System OnTrigger trg grd trgts rslvs
  where
    trg = oneOf (enteredPhase Seige)
        . oneOf (enteredPhase Retaliate)
        $ (enteredPhase Nominate)

    grd = alwaysOk
    
    trgts = targetRuleCard . Map.keys $ rslvs
    rslvs = Map.fromList [(TID 0, independent $ set ActiveFlag 1)]

nominatePhase :: Ability
nominatePhase = Ability Player OnResolve trg grd trgts rslvs
  where
    trg = both clearStack
        . both clearCurrent
        . both activeSet
        $ inPhase Nominate
    grd = oneOf isRulesCard $ inZone Barrack

    trgts = convert [0] validNoms
    rslvs = Map.fromList
            [(TID 0, orSetUnactive . independent $ set NominateFlag 1)]

formationPhase :: Ability
formationPhase = Ability System OnResolve trg grd trgts rslvs
  where
    trg = enteredPhase Formation
    grd = alwaysOk

    trgts = convert (repeat 0) getNominated
    rslvs = Map.fromList
            [(TID 0, independent $ set Zone (fromEnum Battlefield))]

retreatPhase :: Ability
retreatPhase = Ability System OnResolve trg grd trgts rslvs
  where
    trg = enteredPhase Retreat
    grd = alwaysOk

    trgts = convert (repeat 0) (getZone Battlefield)

    rslvs = Map.fromList [(TID 0, independent $ set Zone (fromEnum Barrack))]

nightPhase :: Ability
nightPhase = Ability System OnResolve trg grd trgts rslvs
  where
    trg = enteredPhase Night
    grd = alwaysOk

    trgts1 = convert (repeat 0) (getZone Barrack)
    trgts2 = convert (repeat 1) (getZone Barrack)
    trgts = trgts1 <> trgts2

    rslvs = Map.fromList
            [ (TID 0, independent $ shift (Stat Fatigued) (-1))
            , (TID 1, independent $ set NominateFlag 0)
            ]
