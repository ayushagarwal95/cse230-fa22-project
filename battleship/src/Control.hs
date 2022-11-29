{-# LANGUAGE OverloadedStrings #-}
module Control (handleEvent) where 

import Brick.Main ( continue, halt )
import Brick.Types ( BrickEvent(VtyEvent, AppEvent), EventM, Next )
import qualified Graphics.Vty as V

import State (State(..), Event(..))
import Network (NetworkEvent(..))
import Transition (transition)
import Control.Monad.IO.Class ( MonadIO(liftIO) )


-- HandleEvent maps external events to state transition events 
-- and processes them accordingly
handleEvent :: State -> BrickEvent n NetworkEvent -> EventM n (Next State)
handleEvent st (AppEvent Network.Ready)            = Control.transition st State.Ready
handleEvent st (AppEvent (Network.Attack c))       = Control.transition st (State.Attack c)
handleEvent st (AppEvent Network.Hit)              = Control.transition st State.Hit
handleEvent st (AppEvent Network.Miss)             = Control.transition st State.Miss
handleEvent st (VtyEvent (V.EvKey V.KEnter _))     = Control.transition st State.Enter
handleEvent st (VtyEvent (V.EvKey (V.KChar ch) _)) = Control.transition st $ State.Character ch
handleEvent st (VtyEvent (V.EvKey V.KBS _))        = Control.transition st State.Backspace
handleEvent st (VtyEvent (V.EvKey V.KEsc _))       = halt $ Quit (game st)
handleEvent st _                                   = continue st

transition :: State -> Event -> EventM n (Next State)
transition st e = do x <- liftIO $ Transition.transition st e
                     case x of 
                       Just nxt -> continue nxt
                       Nothing  -> error "Error"