module Control (handleEvent) where 

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Brick.Main ( continue, halt )
import Brick.Types ( BrickEvent(VtyEvent, AppEvent), EventM, Next )
import qualified Graphics.Vty as V

import State (State(..), Event(..))
import Network (NetworkEvent(..), Addr(..), sendEvent)


-- HandleEvent maps external events to state transition events 
-- and processes them accordingly
handleEvent :: State -> BrickEvent n NetworkEvent -> EventM n (Next State)
handleEvent s (AppEvent Network.Flip)              = do transition s State.Flip 
handleEvent s (VtyEvent (V.EvKey V.KEnter _))      = do transition s State.Enter
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt s
handleEvent s _                                    = continue s


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
