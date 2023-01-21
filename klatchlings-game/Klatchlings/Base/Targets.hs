module Base.Targets
  ( Targets(..)
  , Targeting
  , TargetMap
  , TargetID(..)
  , Target(..)
  , Range
  , combine
  , convert
  , targetRuleCard
  , toTargets
  , toDraw
  , validPlays
  , validNoms
  , getNominated
  , getZone'
  , targetSelf
  , targetHero
  , aimStrikes
  ) where

import Internal.Types
  ( Targets(..)
  , TargetMap
  , TargetID(..)
  , Target(..)
  , Range
  , CardID(..)
  )

import Base.Fields
import Base.GameState

import Logic.Battle

type Targeting = CardID -> GameState -> [Target]

combine :: Targeting -> Targeting -> Targeting
combine f g cID gs = (++) (f cID gs) (g cID gs)

convert :: [Int] -> Targeting -> Targets
convert tIDs f = Targets $ \cID gs -> zip (map TID tIDs) . f cID $ gs

instance Semigroup Targets where
  (<>) (Targets trgts1) (Targets trgts2)
    = Targets $ \cID gs -> (++) (trgts1 cID gs) (trgts2 cID gs)

targetRuleCard :: [TargetID] -> Targets
targetRuleCard tIDs = Targets $ \_ _ ->
  zip tIDs . repeat . Given . CardID $ 0

toTargets :: Targeting -> Targets
toTargets f = Targets $ \cID gs -> zip (map TID [0..]) . f cID $ gs

toDraw :: Owner -> Targeting
toDraw owner _ gs
  = let owned = refine (Attr Owner) ((==) owner . toEnum)
              . getCS $ gs
        topDeck = within 
                . refine (Attr Zone) ((==) TopDeck . toEnum)
                $ owned
        midDeck = within
                . refine (Attr Zone) ((==) MidDeck . toEnum)
                $ owned
        botDeck = within
                . refine (Attr Zone) ((==) BotDeck . toEnum)
                $ owned
     in case (topDeck, midDeck, botDeck) of
          ([], [], []) -> []
          ([], [], xs) -> [Given $ head xs]
          ([], xs, _) -> [Random $ xs]
          (xs, _, _) -> [Given $ head xs]

validPlays :: Phase -> Targeting
validPlays p _ gs
  = let cs = getCS gs
        active = case (p, retreive (CardID 0) (Attr AttackFlag) cs) of
                   (Seige, 0) -> P1
                   (Seige, 1) -> P2
                   (Retaliate, 0) -> P2
                   (Retaliate, 1) -> P1
                   _ -> undefined
        mana = retreive (getHero active cs) (Attr Mana) cs
        hand = within
             . refine (Stat Cost) (mana >)
             . refine (Attr Owner) ((==) (fromEnum active))
             . refine (Attr Zone) ((==) (fromEnum Hand))
             . getCS $ gs
     in case hand of
          [] -> [Given . CardID $ 0]
          xs -> [Inquire . (:) (CardID 0) $ xs]

validNoms :: Targeting
validNoms _ gs
  = let barracks
          = within
          . refine (Stat Fatigued) (0 <=)
          . refine (Attr NominateFlag) ((/=) 1)
          . refine (Attr Zone) ((==) Barrack . toEnum)
          . getCS $ gs
     in case barracks of
          [] -> [Given . CardID $ 0]
          xs -> [Inquire . (:) (CardID 0) $ xs]

getNominated :: Targeting
getNominated _ gs
  = map Given
  . within
  . refine (Attr NominateFlag) ((==) 1)
  . refine (Attr Zone) ((==) Barrack . toEnum)
  . getCS $ gs

getZone' :: Zone -> Targeting
getZone' z _ gs
  = map Given
  . getZone z
  . getCS $ gs

targetSelf :: Targeting
targetSelf cID _
  = [Given cID]

targetHero :: Targeting
targetHero cID gs = [Given . getHero owner . getCS $ gs]
  where
    owner = toEnum . retreive cID (Attr Owner) . getCS $ gs

aimStrikes :: Targeting
aimStrikes cID
  = map (\(_, tcID, _) -> Given tcID)
  . filter f . constructStrikes . getCS
    where f (scID, _, _) = cID == scID
