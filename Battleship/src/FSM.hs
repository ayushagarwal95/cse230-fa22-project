module FSM where

import Battleship (Game, Coordinate)


data GameState
    = Start              -- Start state of P1 (Waiting for connection from P2)
    | Connect            -- Start state of P2 (Attempting to connect to P1)
    | Setup Game         -- Setting up the board
    | Attacking Game     -- Player's turn to attack
    | Waiting Game       -- Player is waiting for opponent to attack
    | Win                -- Player won
    | Lose               -- Player lost
    deriving (Eq, Show)


data GameEvent
    = Position Coordinate  -- Position of bottom-left 
    | Attack Coordinate    -- Attack coordinate
    | Ready                -- Signal that player is ready
    deriving (Eq, Show)
