module Transition (transition) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Brick.Main ( continue )
import Brick.Types ( EventM, Next )
import State (State(..), Event(..))
import Network (NetworkEvent(..), Addr(..), sendEvent)


-- Transition performs the actual transition of states of a given event
transition :: State -> Event -> EventM n (Next State)
transition s State.Flip  = handleFlip s
transition s Enter = handleEnter s

handleFlip :: State -> EventM n (Next State)
handleFlip (A c) = continue $ B c
handleFlip (B c) = continue $ A c

handleEnter :: State -> EventM n (Next State)
handleEnter s = do
  liftIO $ sendEvent (port (conn s)) Network.Flip
  continue s
