{-# LANGUAGE OverloadedStrings #-}

module UI (attrs, draw) where

import Brick.AttrMap (AttrMap, AttrName, attrMap)
import Brick.Types (Padding (..), Widget)
import Brick.Util (fg, on)
import Brick.Widgets.Border (border, borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Core (fill, hBox, hLimit, joinBorders, padAll, padLeftRight, padRight, setAvailableSize, str, textWidth, txt, txtWrap, vBox, withAttr, withBorderStyle)
import Brick.Widgets.Table (ColumnAlignment (AlignCenter), RowAlignment (..), alignCenter, alignLeft, renderTable, setDefaultColAlignment, setDefaultRowAlignment, table)
import Data.Bits (xor)
import Data.Char (chr, ord)
import qualified Data.Text as T
import Game (Coordinate, Game, Status (..), getOceanStatus, getTargetStatus, isHit, isSetup)
import qualified Graphics.Vty as V
import State (State (..))
import Types (ResourceName)

-- Returns the attribute style map for the application
attrs :: AttrMap
attrs =
  attrMap
    V.defAttr
    [ (errorAttr, fg V.red),
      (boatAttr, V.blue `on` V.blue),
      (hitAttr, V.red `on` V.red),
      (missAttr, V.white `on` V.white)
    ]

-- Draws the TUI from the given state
draw :: State -> [Widget ResourceName]
draw st = [draw' st]

draw' :: State -> Widget ResourceName
draw' st =
  vBox $
    let grids = drawGrids st
        txtBoxes = drawTextBoxes st
     in [grids, txtBoxes]

-- Draws the grids
drawGrids :: State -> Widget ResourceName
drawGrids st =
  let g = [drawGrid st oceanGrid]
      g' = [drawGrid st targetGrid]
   in hBox $ g ++ g'

-- Draws the grid
drawGrid :: State -> String -> Widget ResourceName
drawGrid st h =
  withBorderStyle unicode $
    borderWithLabel (padLeftRight 1 $ str h) $
      let grid = constructGrid
          oGrid = gridMap (transform st) grid -- Add attributes here
          tGrid = gridMap (transform' st) grid -- Add attributes here
          wGrid = if h == oceanGrid then oGrid else tGrid
          widget =
            padRight (Pad 1) $
              renderTable $
                setDefaultRowAlignment AlignMiddle $
                  setDefaultColAlignment AlignCenter $
                    table wGrid
       in widget

-- Construct grid
constructGrid :: [[Coordinate]]
constructGrid = map f [-1 .. 9]
  where
    f i = [(i, j) | j <- [-1 .. 9]]

-- Map all values in a 2d-grid
gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap f xs = map (\x -> map f x) xs

-- Transforms Coordinates to widgets (Ocean)
transform :: State -> Coordinate -> Widget ResourceName
transform st (r, c) =
  if (r == -1) `xor` (c == -1)
    then
      if r == -1
        then str $ show c
        else str [chr $ ord 'a' + r]
    else
      padLeftRight 1 $
        withAttr (mapStatus $ getOceanStatus (r, c) $ game st) drawEmpty

-- Transforms Coordinates to widgets (Target)
transform' :: State -> Coordinate -> Widget ResourceName
transform' st (r, c) =
  if (r == -1) `xor` (c == -1)
    then
      if r == -1
        then str $ show c
        else str [chr $ ord 'a' + r]
    else
      padLeftRight 1 $
        withAttr (mapStatus $ getTargetStatus (r, c) $ game st) drawEmpty

-- Draws an empty cell
drawEmpty :: Widget ResourceName
drawEmpty = str "  "

-- Draws the textboxes
drawTextBoxes :: State -> Widget ResourceName
drawTextBoxes st =
  hBox
    [ drawTextStyle input $ drawTextBox st,
      drawTextStyle info $ drawInfoBox st
    ]

-- Adds Textbox Styling
drawTextStyle :: String -> Widget ResourceName -> Widget ResourceName
drawTextStyle h w =
  withBorderStyle unicode $
    borderWithLabel (padLeftRight 1 $ str h) $
      padLeftRight 1 $
        setAvailableSize
          (55, 15)
          w

-- Draws the textbox
drawTextBox :: State -> Widget ResourceName
drawTextBox st =
  vBox
    [ drawPrompt st,
      fill ' '
    ]

-- Draw Info
drawInfoBox :: State -> Widget ResourceName
drawInfoBox st =
  vBox
    [drawInfo st, fill ' ']

-- Draws Text Prompt for the current state
drawPrompt :: State -> Widget ResourceName
drawPrompt (Setup1 _ _ s e) = vBox [setupPrompt e, drawInput s]
drawPrompt (Setup2 _ _ s e) = vBox [setupPrompt e, drawInput s]
drawPrompt (Start _ _) = str connect
drawPrompt (SetupWait _ _) = str connect
drawPrompt (Waiting _ _) = str waiting
drawPrompt (AttackWait _ _ _) = str attackWait
drawPrompt (Attacking _ _ s e) = vBox [attackingPrompt e, drawInput s]
drawPrompt (Win _) = str "You Won!"
drawPrompt (Lose _) = str "You Lost."

setupPrompt :: Bool -> Widget n
setupPrompt False = str setup
setupPrompt _ = vBox [errorPrompt invalidInput, str setup]

errorPrompt :: String -> Widget n
errorPrompt s = vBox [withAttr errorAttr $ str s, str " "]

attackingPrompt :: Bool -> Widget n
attackingPrompt False = str attacking
attackingPrompt _ = vBox [errorPrompt invalidInput, str attacking]

-- Draws Prompt
drawInput :: String -> Widget ResourceName
drawInput sr = str $ ">> " ++ sr

-- Draws Info Text for the current state
drawInfo :: State -> Widget ResourceName
drawInfo (Setup1 _ g _ _) = setupInfo g
drawInfo (Setup2 _ g _ _) = setupInfo g
drawInfo (Start _ _) = generalInfo
drawInfo (SetupWait _ _) = setupWaitInfo
drawInfo (Waiting _ _) = generalInfo
drawInfo (Attacking _ _ _ _) = attackingInfo
drawInfo (AttackWait _ _ _) = generalInfo
drawInfo _ = generalInfo

-- Attributes
aAttr, bAttr :: AttrName
aAttr = "aAttr"
bAttr = "bAttr"

boatAttr, hitAttr, missAttr, emptyAttr :: AttrName
boatAttr = "boatAttr"
hitAttr = "hitAttr"
missAttr = "missAttr"
emptyAttr = "emptyAttr"

errorAttr :: AttrName
errorAttr = "error"

-- Headers
oceanGrid, targetGrid, input, info :: String
oceanGrid = "Ocean Grid"
targetGrid = "Target Grid"
input = "Input"
info = "Info"

-- Prompts
setup :: String
setup = "(Setup) Position a boat:"

connect :: String
connect = "(Ready) Waiting for the opponent to connect..."

attacking :: String
attacking = "(Attack) Enter a coordinate:"

attackWait :: String
attackWait = "(Attack) Waiting for outcome of attack..."

waiting :: String
waiting = "(Wait) Waiting for the opponent to attack..."

-- Info
generalInfo :: Widget n
generalInfo = str "ESC - quit"

readyInfo :: Widget n
readyInfo = str "r - ready"

setupInfo :: Game -> Widget n
setupInfo g =
  vBox
    [ generalInfo,
      readyLine g,
      str " ",
      str "Input Format - b:o:c",
      str "  b - boat number ([1..5])",
      str "  o - orientation (v/h)",
      str "  c - top-left coordinate  (rc)",
      str " ",
      boatInfo
    ]
  where
    readyLine g = if isSetup g then readyInfo else str " "

attackingInfo :: Widget n
attackingInfo =
  vBox
    [ generalInfo,
      str " ",
      str " ",
      str "Input Format - c",
      str "  c - attack coordinate  (rc)"
    ]

boatInfo :: Widget n
boatInfo =
  vBox
    [ str "Boats:",
      str "  1 - (1x2)",
      str "  2 - (1x3)",
      str "  3 - (1x3)",
      str "  4 - (1x4)",
      str "  5 - (1x5)"
    ]

setupWaitInfo :: Widget n
setupWaitInfo =
  vBox
    [ generalInfo,
      str "ENTER - try to reconnect"
    ]

-- Error Message
invalidInput :: String
invalidInput = "Error - Invalid Input"

-- Mappers
mapStatus :: Status -> AttrName
mapStatus H = hitAttr
mapStatus M = missAttr
mapStatus B = boatAttr
mapStatus E = emptyAttr