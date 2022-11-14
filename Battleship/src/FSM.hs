module FSM where

import Battleship (Game, Coordinate, Boat, attack, terminate, empty, addShip, isSetup)


data GameState
    = Setup1 Game        -- P1 Setting up their board
    | Setup2 Game        -- P2 Setting up their board
    | Start Game         -- P1 has finished setup and is now ready for P2
    | SetupWait Game     -- P2 has finished setup and is waiting for P1
    | Attacking Game     -- Player's turn to attack
    | Waiting Game       -- Player is waiting for opponent to attack
    | Win                -- Player won
    | Lose               -- Player lost
    deriving (Eq, Show)


data GameEvent
    = Position Boat Coordinate  -- Position Boat
    | Attack Coordinate         -- Attack coordinate
    | Ready                     -- Signal 
    | Connect                   -- Signal that player is ready
    deriving (Eq, Show)

-- Wrapper that returns the previous state if an error was encountered
next :: GameState -> GameEvent -> GameState
next gs ge = case transition gs ge of 
    Just gs' -> gs'
    Nothing -> gs


transition :: GameState -> GameEvent -> Maybe GameState
transition (Setup1 g) (Position b c) = handleSetup True g b c
transition (Setup2 g) (Position b c) = handleSetup False g b c
transition (Setup1 g) Ready          = handleReady1 g
transition (Setup2 g) Ready          = handleReady2 g
transition (Start g) Connect         = Just (Attacking g)
transition (SetupWait g) Connect     = Just (Waiting g)
transition (Attacking g) (Attack c)  = handleAttack g c
transition (Waiting g) (Attack c)    = handleWait g c
transition gs _                      = Nothing


-- 1) register attack 
-- 2) notify opponent of attack
-- 3) if it's a hit, register the hit (hits)
-- 4) win if all ships are destroyed else go to waiting
handleAttack :: Game -> Coordinate -> Maybe GameState
handleAttack g c = do g' <- attack c g
                      -- TODO: Add networking
                      if terminate g' then 
                          return Win
                      else
                          return (Waiting g')


-- 1) register opponents attack 
-- 2) notify opponent of result
-- 3) if it's a hit, register the hit (damaged)
-- 4) lost if all ships are destroyed else go to attacking
handleWait :: Game -> Coordinate -> Maybe GameState
handleWait g c = error "todo"


handleSetup :: Bool -> Game -> Boat -> Coordinate -> Maybe GameState
handleSetup p1 g b c = do g' <- addShip b c g 
                          if p1 then return (Setup1 g') else return (Setup2 g')


-- Validate if the game is ready and then transition
handleReady1 :: Game -> Maybe GameState
handleReady1 g = if isSetup g then Just (Start g) else Nothing


-- Validate if the game is ready and then transition
-- Should check if P1 is ready, if so then go straight to `Waiting`, 
-- else go to `SetupWait`
handleReady2 = error "todo"
