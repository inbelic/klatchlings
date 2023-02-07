module Base.Guard
  ( Guard(..)
  , both
  , oneOf
  , emptyStack
  , inZone
  , alwaysOk
  , isRulesCard
  ) where

import Internal.Boolean
import Internal.Types
  ( Guard(..)
  )

import Base.Fields
import Base.GameState

instance Boolean Guard where
  both (Guard grd1) (Guard grd2)
   = Guard $ \cID tcID gs -> grd1 cID tcID gs
                          && grd2 cID tcID gs
  oneOf (Guard grd1) (Guard grd2)
    = Guard $ \cID tcID gs -> grd1 cID tcID gs
                           || grd2 cID tcID gs

emptyStack :: Guard
emptyStack = Guard $ \_ _ -> null . getStack

inZone :: Zone -> Guard
inZone z = Guard $ \_ tcID ->
  (==) z . toEnum . retreive tcID (Attr Zone) . getCS

alwaysOk :: Guard
alwaysOk = Guard $ \_ _ _ -> True

isRulesCard :: Guard
isRulesCard = Guard grd
  where grd _ (CardID 0) _ = True
        grd _ _ _ = False
