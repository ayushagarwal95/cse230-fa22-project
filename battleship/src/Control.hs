module Control (handleEvent) where 

import Brick.Main ( continue, halt )
import Brick.Types ( BrickEvent(VtyEvent, AppEvent), EventM, Next )
import qualified Graphics.Vty as V

import State (State(..), Event(..))
import Network (NetworkEvent(..))
import Transition (transition)


-- HandleEvent maps external events to state transition events 
-- and processes them accordingly
handleEvent :: State -> BrickEvent n NetworkEvent -> EventM n (Next State)
handleEvent s (AppEvent Network.Flip)              = do transition s State.Flip 
handleEvent s (VtyEvent (V.EvKey V.KEnter _))      = do transition s State.Enter
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt s
handleEvent s _                                    = continue s
