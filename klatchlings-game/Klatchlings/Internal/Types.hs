module Internal.Types where

import Internal.Fields

import qualified Data.Map as Map (Map)

newtype CardID = CardID
  { cardID :: Int
  }
  deriving (Eq, Ord)

newtype AbilityID = AbilityID
  { abltyID :: Int
  }
  deriving (Eq, Ord)

type AbilityMap = Map.Map AbilityID Ability

type AttrMap = Map.Map Attr Int
type AttrState = Map.Map CardID AttrMap

type StatMap = Map.Map Stat Status
type StatMap' = Map.Map Stat Int
type StatState = Map.Map Stat StatMap'

type FieldMap = Map.Map Field Int
type FieldState = Map.Map CardID FieldMap
type CardState = FieldState

newtype Status = Status
  { status :: AttrState -> Int -> Int
  }

instance Semigroup Status where
  (<>) (Status f) (Status g)
    = Status $ \as -> f as . g as

data Ability = Ability
