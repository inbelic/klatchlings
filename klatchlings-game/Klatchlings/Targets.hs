module Targets
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
  ) where

import Internal.Types
  ( Targets(..)
  , TargetMap
  , TargetID(..)
  , Target(..)
  , Range
  , CardID(..)
  )

import Fields
import GameState

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
toDraw owner cID gs
  = let owned = refine (Attr Owner) ((==) owner . toEnum)
              $ getCS $ gs
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
