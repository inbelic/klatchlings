module Internal.Types where

import Internal.Fields

import qualified Data.Map as Map (Map)

-- Card related things
data Card = Card
  { attrs       :: AttrMap
  , stats       :: StatMap
  , abilities   :: [Ability]
  , original    :: Maybe Card
  }

type Cards = Map.Map CardID Card

newtype Change = Change
  { change :: Card -> (Card, Alteration)
  }

data Alteration
  = Shift Field Int     -- We shifted the Field value by Int
  | Set Attr Int        -- We set an Attr to a value Int
  | Replace Stat        -- We modified the Status of a Stat
  | Equip               -- We gave the card an ability

newtype CardID = CardID
  { cardID :: Int
  }
  deriving (Eq, Ord)

type AttrMap = Map.Map Attr Int
type AttrState = Map.Map CardID AttrMap

type StatMap = Map.Map Stat Status
type StatMap' = Map.Map Stat Int
type StatState = Map.Map CardID StatMap'

type FieldMap = Map.Map Field Int
type FieldState = Map.Map CardID FieldMap
type CardState = FieldState

newtype Status = Status
  { status :: AttrState -> Int -> Int
  }

instance Semigroup Status where
  (<>) (Status f) (Status g)
    = Status $ \as -> f as . g as


-- Ability related things
data Ability = Ability Timing Trigger Guard Targets Changes

newtype Trigger = Trigger
  { trigger :: CardID -> GameState -> Bool
  }

data Timing
  = OnResolve
  | OnTrigger
  deriving Eq

newtype Targets = Targets
  { getTargets :: CardID -> GameState -> TargetMap
  }

newtype TargetID = TargetID
  { targetID :: Int
  }
  deriving (Eq, Ord)

type TargetMap = [(TargetID, Target)]
type Changes = Map.Map TargetID Change

newtype Guard = Gurad
  { guard :: CardID -> CardID -> GameState -> Bool
  }

type Range = [CardID]
data Target
  = Given CardID
  | Inquire Range
  | Random Range
  | Void
  deriving (Eq, Ord)

data Game = Game Stack History Cards
data GameState = GS Stack History CardState

type Stack = [Header]

data Header
  = Assigned CardID Guard [(Change, Target)]
  | Unassigned CardID Guard Targets Changes
  | Targeted CardID Guard [(Change, CardID)]

-- History related things
newtype History = History
  { history :: ([Page], [Page])
  }

data Page = Page CardID CardID Alteration
