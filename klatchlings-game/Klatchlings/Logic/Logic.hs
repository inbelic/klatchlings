module Logic.Logic where

import qualified Data.Map as Map (fromList)

import Base.Fields
import Base.GameState (CardID(..))
import Logic.Keywords
import Logic.Rules

cards = Map.fromList
  [ rules
  , (CardID 1, dummyUnit 1 P1)
  , (CardID 2, dummyUnit 1 P2)
  ]
