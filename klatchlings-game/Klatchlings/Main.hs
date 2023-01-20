module Main where

import Game

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)

import Internal.Harness (tcpHarness)

runGame :: Chan String -> IO ()
runGame ch = do
  g' <- startGame ch
  putStrLn "game finished.."

main :: IO ()
main = do
  ch <- newChan
  forkIO (runGame ch)
  tcpHarness ch
