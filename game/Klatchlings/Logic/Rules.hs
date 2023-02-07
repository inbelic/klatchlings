module Logic.Rules where

import qualified Data.Map as Map
  ( Map, empty, fromList, keys
  )

import Base.Ability
import Base.Fields
import Base.Card
import Base.Resolves
import Base.GameState
import Base.Status

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
    rslvs = Map.fromList [(TID 0, moveZone Hand)]

play :: Phase -> Ability
play p = Ability System OnResolve trg grd trgts rslvs
  where
    trg = both clearStack . both clearCurrent . both activeSet $ inPhase p 
    grd = oneOf isRulesCard $ inZone Hand

    trgts = convert [0] (validPlays p)
    rslvs = Map.fromList
            [(TID 0, orSetUnactive $ moveZone Stack)]

setActive :: Ability
setActive = Ability System OnTrigger trg grd trgts rslvs
  where
    trg = oneOf (enteredPhase Seige)
        . oneOf (enteredPhase Retaliate)
        $ enteredPhase Nominate

    grd = alwaysOk
    
    trgts = targetRuleCard . Map.keys $ rslvs
    rslvs = Map.fromList [(TID 0, independent $ set ActiveFlag 1)]

nominatePhase :: Ability
nominatePhase = Ability System OnResolve trg grd trgts rslvs
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
            [(TID 0, moveZone Battlefield)]

skirmishPhase :: Ability
skirmishPhase = Ability System OnResolve trg grd trgts rslvs
  where
    trg = enteredPhase Skirmish
    grd = alwaysOk

    trgts = undefined

    rslvs = undefined

retreatPhase :: Ability
retreatPhase = Ability System OnResolve trg grd trgts rslvs
  where
    trg = enteredPhase Retreat
    grd = alwaysOk

    trgts1 = targetRuleCard [TID 0]
    trgts2 = convert (repeat 1) (getZone' Battlefield)
    trgts = trgts1 <> trgts2

    rslvs = Map.fromList [(TID 0, setAttackFlag), (TID 1, moveZone Barrack)]

nightPhase :: Ability
nightPhase = Ability System OnResolve trg grd trgts rslvs
  where
    trg = enteredPhase Night
    grd = alwaysOk

    trgts1 = convert (repeat 0) (getZone' Barrack)
    trgts2 = convert (repeat 1) (getZone' Barrack)
    trgts = trgts1 <> trgts2

    rslvs = Map.fromList
            [ (TID 0, independent $ shift (Stat Fatigued) (-1))
            , (TID 1, independent $ set NominateFlag 0)
            ]
