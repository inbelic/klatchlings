module Trigger
  ( Trigger(..)
  , both
  , oneOf
  , clearStack
  , clearCurrent
  , enteredPhase
  ) where

import History
import Internal.Boolean
import Internal.Types
  ( Trigger(..)
  , Page(..)
  , Alteration(..)
  )

import Fields
import GameState

instance Boolean Trigger where
  both (Trigger trg1) (Trigger trg2)
   = Trigger $ \cID gs -> trg1 cID gs && trg2 cID gs
  oneOf (Trigger trg1) (Trigger trg2)
    = Trigger $ \cID gs -> trg1 cID gs || trg2 cID gs

clearStack :: Trigger
clearStack = Trigger $ \_ -> null . getStack

clearCurrent :: Trigger
clearCurrent = Trigger $ \_ -> null . current . getHistory

enteredPhase :: Phase -> Trigger
enteredPhase p = Trigger $ \_ -> any f . current . getHistory
  where f (Page _ (CardID 0) (Set Phase cp)) = (==) cp . fromEnum $ p
        f _ = False
