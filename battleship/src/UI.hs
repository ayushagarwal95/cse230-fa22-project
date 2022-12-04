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

-- Attributes

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

-- Mappers
mapStatus :: Status -> AttrName
mapStatus H = hitAttr
mapStatus M = missAttr
mapStatus B = boatAttr
mapStatus E = emptyAttr
