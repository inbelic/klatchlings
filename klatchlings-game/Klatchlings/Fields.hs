module Fields where

data Field
  = Attr Attr
  | Stat Stat
  deriving (Eq, Ord)

instance Show Field where
  show (Attr atr) = show atr
  show (Stat stt) = show stt

data Attr
    = Owner | Zone                  -- Enum Attrs
    | Mana                          -- Int Attrs
    | ActiveFlag | AttackFlag       -- Boolean flags
    | Phase                         -- Rules enums
    deriving (Eq, Ord, Show)

data Stat
    = Power | Toughness | Cost      -- Integer Stats
    | Fatigue                       -- Boolean Stats
    deriving (Eq, Ord, Show)


data Owner = P1 | P2
  deriving (Eq, Ord, Enum)

data Zone
  = Ether | Hand | Stack | Barrack | Grave | Battlefield
  | TopDeck | MidDeck | BotDeck
  deriving (Eq, Ord, Enum)

data Phase = Start | Morning | Seige | Retaliate
           | Nominate | Skirmish | Denominate | Night
  deriving (Eq, Ord, Enum)
