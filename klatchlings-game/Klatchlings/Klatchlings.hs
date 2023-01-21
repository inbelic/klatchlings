module Klatchlings where

import Game

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)

harness :: Chan String -> IO ()
harness ch = do
  contents <- readChan ch
  putStrLn contents
  response <- getLine
  writeChan ch response
  harness ch

runGame :: Chan String -> IO ()
runGame ch = do
  g' <- startGame ch
  putStrLn "game finished.."

main :: IO ()
main = do
  ch <- newChan
  forkIO (runGame ch)
  harness ch
