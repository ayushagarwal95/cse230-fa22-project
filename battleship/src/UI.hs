{-# LANGUAGE OverloadedStrings #-}
module UI (attrs, draw) where 

import Brick.Types ( Widget )
import Brick.Widgets.Core ( str, withAttr )
import Brick.AttrMap ( AttrMap, AttrName, attrMap )
import Brick.Util ( on )
import qualified Graphics.Vty as V

import State (State(..))
import Types (ResourceName)


-- Returns the attribute style map for the application
attrs :: AttrMap
attrs = attrMap V.defAttr 
  [ (aAttr, V.blue `on` V.blue)
  , (bAttr, V.red `on` V.red)
  ]

-- Draws the TUI from the given state
draw :: State -> [Widget ResourceName]
draw (A _) = [withAttr aAttr drawCell]
draw (B _) = [withAttr bAttr drawCell]

-- Draws an empty cell
drawCell :: Widget n 
drawCell = str "  "

-- Attributes
aAttr, bAttr :: AttrName
aAttr = "aAttr"
bAttr = "bAttr"
