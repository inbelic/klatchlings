module Game
  ( Game
  , startGame
  ) where

import Internal.Types (Game(..), CardID(..))
import Internal.Engine (resolveStack)

import Base.History
import Base.GameState

import Logic.Logic

import Control.Concurrent.Chan
import Control.Monad (liftM)

import qualified Data.Map as Map

startGame :: Chan String -> IO Game
startGame ch = do
  let game = Game [] begin cards
  putStrLn . show . getCS . peek $ game
  resolveStack ch $ game
