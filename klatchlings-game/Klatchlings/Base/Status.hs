module Base.Status
  ( Status(..)
  , strictSet
  ) where

import Internal.Types (Status(..))

strictSet :: Int -> Status
strictSet x = Status $ const (const x)
