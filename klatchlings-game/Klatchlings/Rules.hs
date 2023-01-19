module Rules where

import qualified Data.Map as Map
  ( Map, empty, fromList, keys
  )

import Ability
import Fields
import Card
import Resolves
import GameState

cards = Map.fromList
  [ rules
  , (CardID 1, dummyUnit P1)
  , (CardID 2, dummyUnit P2)
  ]

dummyUnit :: Owner -> Card
dummyUnit owner
  = mint
  . fst . set Owner (fromEnum owner)
  . fst . set Zone (fromEnum MidDeck)
  $ blank

rules :: (CardID, Card)
rules = (CardID 0, crd)
  where crd = mint
            . fst . equip setActive
            . fst . (equip $ play Retaliate)
            . fst . (equip $ play Seige)
            . fst . equip morningPhase
            . fst . equip phaseControl
            . fst . set Phase (fromEnum Start)
            . fst . set ActiveFlag 0
            . fst . set AttackFlag 0
            $ blank


phaseControl :: Ability
phaseControl = Ability OnTrigger trg grd trgts rslvs
  where
    trg = both clearStack clearCurrent
    grd = emptyStack

    trgts = targetRuleCard . Map.keys $ rslvs
    rslvs = Map.fromList [(TID 0, advancePhase)]

morningPhase :: Ability
morningPhase = Ability OnResolve trg grd trgts rslvs
  where
    trg = enteredPhase Morning
    grd = oneOf (inZone TopDeck)
        . oneOf (inZone MidDeck)
        $ inZone BotDeck

    trgts = convert [0, 0] . combine (toDraw P1) $ (toDraw P2)
    rslvs = Map.fromList [(TID 0, independent $ set Zone (fromEnum Hand))]

play :: Phase -> Ability
play p = Ability OnResolve trg grd trgts rslvs
  where
    trg = both clearStack . both clearCurrent . both activeSet $ inPhase p 
    grd = oneOf isRulesCard $ inZone Hand

    trgts = convert [0] (validPlays p)
    rslvs = Map.fromList
            [(TID 0, orSetUnactive . independent $ set Zone (fromEnum Stack))]

setActive :: Ability
setActive = Ability OnTrigger trg grd trgts rslvs
  where
    trg = oneOf (enteredPhase Seige)
        . oneOf (enteredPhase Retaliate)
        $ (enteredPhase Nominate)

    grd = alwaysOk
    
    trgts = targetRuleCard . Map.keys $ rslvs
    rslvs = Map.fromList [(TID 0, independent $ set ActiveFlag 1)]
