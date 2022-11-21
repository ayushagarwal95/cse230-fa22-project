module Main (main) where

import System.Environment ( getArgs )
import Control.Concurrent (forkIO)

import Brick.Main ( customMain, App(..) )
import Brick.BChan ( newBChan )
import qualified Graphics.Vty as V

import Types (ResourceName)
import State (State, initState)
import Network (NetworkEvent, listen)
import Control (handleEvent)
import UI (draw, attrs)


-- args: <0/1 - P1 or P2> <Listening Port> <Opponent's Port>
main :: IO ()
main = do
  args <- getArgs
  chan <- newBChan 10
  _ <- forkIO $ listen (args !! 1) chan
  initialState <- buildInitialState (head args) (args !! 2)
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  endState <- customMain initialVty buildVty (Just chan) app initialState
  print endState

-- Constructs the start state of the application
buildInitialState :: String -> String -> IO State
buildInitialState "0" p = pure $ initState True p
buildInitialState _   p = pure $ initState False p


-- Brick Application
app :: App State NetworkEvent ResourceName
app =
  App
    { appDraw = draw
    , appChooseCursor = const . const Nothing
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const attrs
    }
