module Base.Fields where

data Field
  = Attr Attr
  | Stat Stat
  deriving (Eq, Ord)

instance Show Field where
  show (Attr atr) = show atr
  show (Stat stt) = show stt

data Attr
    = Owner | Zone                                -- Enum Attrs
    | Mana | Position                             -- Int Attrs
    | ActiveFlag | AttackFlag | NominateFlag      -- Boolean flags
    | Phase                                       -- Rules enums
    deriving (Eq, Ord, Show)

data Stat
    = Power | Toughness | Cost      -- Integer Stats
    | Fatigued                      -- Boolean Stats
    deriving (Eq, Ord, Show)


data Owner = P1 | P2
  deriving (Eq, Ord, Enum)

data Zone
  = Ether | Hand | Stack | Barrack | Grave | Battlefield | Throne
  | TopDeck | MidDeck | BotDeck
  deriving (Eq, Ord, Enum)

data Phase = Start | Morning | Seige | Retaliate | Nominate
           | Formation | Skirmish | Retreat | Night
  deriving (Eq, Ord, Enum)
