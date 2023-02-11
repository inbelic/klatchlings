module Internal.Types where

import Base.Fields (Field, Attr, Stat)

import qualified Data.Map as Map (Map)

-- Card related things
data Card = Card
  { attrs       :: AttrMap
  , stats       :: StatMap
  , abilities   :: AbilityMap
  , original    :: Maybe Card
  }

type Cards = Map.Map CardID Card

type Change = Card -> (Card, Alteration)

data Alteration
  = Shift Field Int     -- We shifted the Field value by Int
  | Set Attr Int        -- We set an Attr to a value Int
  | Replace Stat        -- We modified the Status of a Stat
  | Equip               -- We gave the card an ability
  | Created             -- This card was created as a token
  deriving Show

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
data Ability = Ability Liable Timing Trigger Guard Targets Resolves

newtype AbilityID = AbilityID
  { abilityID :: Int
  }
  deriving (Eq, Ord)

instance Show AbilityID where
  show (AbilityID aID) = show aID

type AbilityMap = Map.Map AbilityID Ability

newtype Trigger = Trigger
  { trigger :: CardID -> GameState -> Bool
  }

data Liable
  = System          -- server will order it
  | Player          -- owner of card will order it
  deriving Eq

instance Show Liable where
  show System = "s"
  show Player = "p"

data Timing
  = OnResolve
  | OnTrigger
  deriving Eq

newtype Targets = Targets
  { getTargets :: CardID -> GameState -> TargetMap
  }

newtype TargetID = TID
  { targetID :: Int
  }
  deriving (Eq, Ord)

type TargetMap = [(TargetID, Target)]
type Resolves = Map.Map TargetID Resolve

newtype Guard = Guard
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
data GameState = GameState
  { getStack    :: Stack
  , getHistory  :: History
  , getCS       :: CardState
  }

type Stack = [Header]

newtype Resolve = Resolve
  { resolve :: CardID -> CardID -> GameState -> Change
  }

data Header
  = Assigned Liable CardID AbilityID Guard [(Resolve, Target)]
  | Unassigned Liable CardID AbilityID Guard Targets Resolves
  | Targeted CardID AbilityID Guard [(Resolve, Maybe CardID)]

instance Show Header where
  show (Assigned lbl cID aID _ _) = show lbl ++ "(" ++ show cID ++ ":" ++ show aID ++ ")"
  show (Unassigned lbl cID aID _ _ _) = show lbl ++ "(" ++ show cID ++ ":" ++ show aID ++ ")"
  show (Targeted cID aID _ _) = "t(" ++ show cID ++ ":" ++ show aID ++ ")"

instance Eq Header where
  (==) (Assigned _ cID1 aID1 _ _) (Assigned _ cID2 aID2 _ _)
    = cID1 == cID2 && aID1 == aID2
  (==) (Unassigned _ cID1 aID1 _ _ _) (Unassigned _ cID2 aID2 _ _ _)
    = cID1 == cID2 && aID1 == aID2
  (==) (Targeted cID1 aID1 _ _) (Targeted cID2 aID2 _ _)
    = cID1 == cID2 && aID1 == aID2
  (==) _ _ = False

getHdrCID :: Header -> CardID
getHdrCID (Assigned _ cID _ _ _) = cID
getHdrCID (Unassigned _ cID _ _ _ _) = cID
getHdrCID (Targeted cID _ _ _) = cID

-- History related things
newtype History = History
  { history :: ([Page], [Page])
  }

data Page = Page CardID CardID Alteration
  deriving Show
