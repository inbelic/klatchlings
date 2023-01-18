module Internal.Types where

import Fields (Field, Attr, Stat)

import qualified Data.Map as Map (Map)

-- Card related things
data Card = Card
  { attrs       :: AttrMap
  , stats       :: StatMap
  , abilities   :: AbilityMap
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

instance Show CardID where
  show (CardID cID) = show cID

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
newtype AbilityID = AbilityID
  { abilityID :: Int
  }
  deriving (Eq, Ord)

instance Show AbilityID where
  show (AbilityID aID) = show aID

type AbilityMap = Map.Map AbilityID Ability
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

type Resolves = [(Change, Maybe CardID)]

data Header
  = Assigned CardID AbilityID Guard [(Change, Target)]
  | Unassigned CardID AbilityID Guard Targets Changes
  | Targeted CardID AbilityID Guard Resolves 

instance Show Header where
  show (Assigned cID aID _ _) = "(" ++ show cID ++ ":" ++ show aID ++ ")"
  show (Unassigned cID aID _ _ _) = "(" ++ show cID ++ ":" ++ show aID ++ ")"
  show (Targeted cID aID _ _) = "(" ++ show cID ++ ":" ++ show aID ++ ")"

instance Eq Header where
  (==) (Assigned cID1 aID1 _ _) (Assigned cID2 aID2 _ _)
    = cID1 == cID2 && aID1 == aID2
  (==) (Unassigned cID1 aID1 _ _ _) (Unassigned cID2 aID2 _ _ _)
    = cID1 == cID2 && aID1 == aID2
  (==) (Targeted cID1 aID1 _ _) (Targeted cID2 aID2 _ _)
    = cID1 == cID2 && aID1 == aID2
  (==) _ _ = False


-- History related things
newtype History = History
  { history :: ([Page], [Page])
  }

data Page = Page CardID CardID Alteration
