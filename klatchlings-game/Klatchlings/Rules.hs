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
  . silently (set Owner (fromEnum owner))
  . silently (set Zone (fromEnum MidDeck))
  $ blank

rules :: (CardID, Card)
rules = (CardID 0, crd)
  where crd = mint
            . silently (equip morningPhase)
            . silently (equip phaseControl)
            . silently (set Phase (fromEnum Start))
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
