module State (State(..), Event(..), initGameState) where 

import Network (Addr(..), initAddr)
import Game (Game, initGame, Coordinate)

-- Possible states of the application
data State 
  = Setup1 { conn :: Addr, game :: Game, s :: String, inv :: Bool }    -- P1 setting up their board
  | Setup2 { conn :: Addr, game :: Game, s :: String, inv :: Bool }    -- P2 setting up their board
  | Start { conn :: Addr, game :: Game }                               -- P1 has finished setup and is now ready for P2
  | SetupWait { conn :: Addr, game :: Game }                           -- P2 has finished setup and is now waiting for P1
  | Attacking { conn :: Addr, game :: Game, s :: String, inv :: Bool } -- Player is currently attacking
  | AttackWait { conn :: Addr, game :: Game, last :: Coordinate }      -- Player is waiting for outcome of attack
  | Waiting { conn :: Addr, game :: Game }                             -- Player is waiting for opponent
  | Win { game :: Game }                                               -- Player won
  | Lose { game :: Game }                                              -- Player Lost
  | Quit { game :: Game }
  deriving (Show)


-- Transition events of the application
data Event
  = Enter
  | Character Char
  | Backspace
  | Ready
  | Attack Coordinate
  | Hit
  | Miss
  deriving (Show, Eq)


-- Construct a new state
initGameState :: Bool -> String -> State
initGameState True p = Setup1 { conn = initAddr p, game = initGame, s = "", inv = False }
initGameState _ p    = Setup2 { conn = initAddr p, game = initGame, s = "", inv = False }
