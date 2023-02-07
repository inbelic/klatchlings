module Game
  ( Game
  , startGame
  ) where

import Internal.Types (Game(..), CardID(..))
import Internal.Engine (resolveStack)
import Internal.Comms (displayState)

import Base.History
import Base.GameState

import Logic.Logic

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Control.Monad (liftM)

import qualified Data.Map as Map

runGame :: Chan String -> IO ()
runGame ch = do
  startInfo <- readChan ch
  let game = Game [] begin cards
  displayState Map.empty ch . peek $ game
  g' <- resolveStack ch game
  return ()

startGame :: IO (Chan String)
startGame = do
  ch <- newChan
  forkIO (runGame ch)
  return ch
