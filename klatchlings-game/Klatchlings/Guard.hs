module Guard
  ( Guard(..)
  , both
  , oneOf
  , emptyStack
  , inZone
  ) where

import Internal.Boolean
import Internal.Types
  ( Guard(..)
  )

import Fields
import GameState

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
