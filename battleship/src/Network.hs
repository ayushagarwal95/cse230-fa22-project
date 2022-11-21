module Network (
  NetworkEvent(..), Addr(..), 
  Network.listen, sendEvent, initAddr) 
  where

import Network.Simple.TCP
    ( connect, recv, send, serve, HostPreference(Host) )
import qualified Data.ByteString.UTF8 as BLU

import Brick.BChan (BChan, writeBChan)


-- Events that are sent over the networking
data NetworkEvent = Flip
  deriving (Read, Show)

-- Addr of the opponent
newtype Addr = Addr { port :: String }
  deriving (Show)

initAddr :: String -> Addr
initAddr p = Addr { port = p}


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
sendEvent :: String -> NetworkEvent -> IO () 
sendEvent p e = connect "127.0.0.1" p f
  where
    f (s, _) = do
      send s (BLU.fromString $ show e)
