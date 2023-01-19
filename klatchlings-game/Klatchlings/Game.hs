module Game
  ( Game
  , startGame
  ) where

import History
import Rules
import Internal.Types (Game(..))
import Internal.Engine (resolveStack)
import Control.Concurrent.Chan

startGame :: Chan String -> IO Game
startGame ch = resolveStack ch $ Game [] begin cards
