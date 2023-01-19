module Resolves
  ( Resolve(..)
  , independent
  , advancePhase
  , orSetUnactive
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
independent chng = Resolve $ \_ _ _ -> chng

advancePhase :: Resolve
advancePhase
  = Resolve $ \_ _ gs ->
    case toEnum . retreive (CardID 0) (Attr Phase) . getCS $ gs of
      Start -> set Phase (fromEnum Morning)
      Night -> set Phase (fromEnum Morning)
      p -> set Phase (fromEnum $ succ p)

orSetUnactive :: Resolve -> Resolve
orSetUnactive (Resolve r) = Resolve rslv
  where rslv _ (CardID 0) _ = set ActiveFlag 0
        rslv cID tcID gs = r cID tcID gs
