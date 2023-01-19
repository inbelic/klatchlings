module Resolves
  ( Resolve(..)
  , independent
  , advancePhase
  ) where

import Internal.Types
  ( Resolve(..)
  , Resolves
  )

import Fields
import GameState
import Card

import qualified Data.Map as Map
  ( fromList
  )

independent :: Change -> Resolve
independent chng = Resolve $ \_ _ -> chng

advancePhase :: Resolve
advancePhase
  = Resolve $ \_ gs ->
    case toEnum . retreive (CardID 0) (Attr Phase) . getCS $ gs of
      Start -> set Phase (fromEnum Morning)
      Night -> set Phase (fromEnum Morning)
      p -> set Phase (fromEnum $ succ p)
