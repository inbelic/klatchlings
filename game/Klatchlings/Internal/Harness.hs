{-# LANGUAGE OverloadedStrings #-}
module Internal.Harness
  ( tcpHarness
  ) where

-- A module that will provide a more coherent interface for the external
-- communication

import Game (startGame)

import qualified Data.Map as Map
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Text.Read (readMaybe)

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)

localHost = "127.0.0.1"
gamePort = "3000"

newtype GameID = GameID
  { gameID :: Int
  }
  deriving (Eq, Ord)

process :: String -> Maybe (GameID, String)
process str =
  let (gIDStr, respStr) = span ((/=) ':') str
   in case (readMaybe gIDStr, respStr) of
        (Just gID, ':' : resp) -> Just (GameID gID, resp)
        _ -> Nothing

prepend :: GameID -> String -> String
prepend (GameID gID) str = show gID ++ ":" ++ str

tcpHarness :: IO ()
tcpHarness = runTCPClient localHost gamePort initialize
  where
    initialize s = do
      sendAll s . C.pack $ "connected"
      harnessLoop Map.empty s

    harnessLoop hm s = do
      contents <- recv s 1024
      case process $ C.unpack contents of
        Nothing -> do
          -- Badly formatted info so we will say so and continue on...
          sendAll s . C.pack $ "bad game id format"
          harnessLoop hm s
        (Just (gID, request)) -> do
          --Otherwise, we can query a response
          ch <- case Map.lookup gID hm of
                  (Just usedCh) -> return usedCh
                  Nothing -> startGame
          putStrLn ("req: " ++ request)
          writeChan ch request
          gameResponse <- readChan ch
          putStrLn ("resp: " ++ gameResponse)
          sendAll s . C.pack . prepend gID $ gameResponse
          let hm' = Map.insert gID ch hm
          harnessLoop hm' s

-- taking from Network.Socket example
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
    where
      resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
      open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
