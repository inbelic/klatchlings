module Internal.Fields where

data Field
  = Attr Attr
  | Stat Stat
  deriving (Eq, Ord)

data Attr
    = Owner | Zone | Mana           -- Enum Attrs
    | ActiveFlag | AttackFlag       -- Boolean flags
    | Phase                         -- Rules enums
    deriving (Eq, Ord)

data Stat
    = Power | Toughness | Cost      -- Integer Stats
    | Fatigue                       -- Boolean Stats
    deriving (Eq, Ord)
