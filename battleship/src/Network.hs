module Network (
  NetworkEvent(..), Addr(..), 
  Network.listen, sendEvent, initAddr, initTestAddr) 
  where

import Network.Simple.TCP
    ( connect, recv, send, serve, HostPreference(Host) )
import qualified Data.ByteString.UTF8 as BLU

import Brick.BChan (BChan, writeBChan)

import Game (Coordinate)


-- Events that are sent over the networking
data NetworkEvent 
  = Ready
  | Attack Coordinate
  | Hit
  | Miss
  deriving (Read, Show)

-- Addr of the opponent
data Addr = Addr { port :: String, test :: Bool }
  deriving (Show)

initAddr :: String -> Addr
initAddr p = Addr { port = p, test = False}

initTestAddr :: Addr
initTestAddr = Addr { port = "8080", test = True }

-- Note: Sockets aren't reused across connections
-- so we can assume that each connection will always correspond 
-- to exactly 1 message

-- Listens for incoming messages and adds them to the Brick Channel
listen :: String -> BChan NetworkEvent -> IO ()
listen p ch = serve (Host "127.0.0.1") p f
  where
    f (s, _) = do
      msg <- m s ""
      writeBChan ch $ read msg
    m s acc = do 
      msg <- recv s 1024
      case msg of 
        Just bs -> do m s (acc ++ BLU.toString bs)
        Nothing -> return acc


-- Sends a network even to the opponent
sendEvent :: Bool -> String -> NetworkEvent -> IO () 
sendEvent t p e = if t then 
                    return ()  -- Hacky thing to handle testing
                  else 
                    connect "127.0.0.1" p f
  where
    f (s, _) = do
      send s (BLU.fromString $ show e)
