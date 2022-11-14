module FSM where

import Battleship (Game, Coordinate, Boat, attack, terminate, empty, addShip, isSetup)


data GameState
    = Start              -- Start state of P1 (Waiting for connection from P2)
    | Connect            -- Start state of P2 (Attempting to connect to P1)
    | Setup1 Game        -- P1 Setting up their board
    | Setup2 Game        -- P2 Setting up their board
    | Ready1 Game        -- P1 has finished setup
    | Ready2 Game        -- P2 has finished setup
    | Attacking Game     -- Player's turn to attack
    | Waiting Game       -- Player is waiting for opponent to attack
    | Win                -- Player won
    | Lose               -- Player lost
    deriving (Eq, Show)


data GameEvent
    = Position Boat Coordinate  -- Position Boat
    | Attack Coordinate         -- Attack coordinate
    | Signal                    -- Signal that player is ready
    | Connected                 -- Signal that players have connected
    | Tick                      -- Refresh ready signal until connected
    deriving (Eq, Show)

-- Wrapper that returns the previous state if an error was encountered
next :: GameState -> GameEvent -> GameState
next gs ge = case transition gs ge of 
    Just gs' -> gs'
    Nothing -> gs


transition :: GameState -> GameEvent -> Maybe GameState
transition Start Connected           = handleStart
transition Connect Tick              = handleConnect
transition Connect Connected         = Just (Setup2 empty)
transition (Setup1 g) (Position b c) = handleSetup True g b c
transition (Setup2 g) (Position b c) = handleSetup False g b c
transition (Setup1 g) Signal         = handleSignal True g
transition (Setup2 g) Signal         = handleSignal False g
transition (Ready1 g) Signal         = handleReady1 g
transition (Ready2 g) Signal         = Just (Waiting g)
transition (Ready2 g) Tick           = handleReady2 g
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

handleSignal :: Bool -> Game -> Maybe GameState
handleSignal p1 g = if isSetup g then 
                        if p1 then return (Ready1 g) else return (Ready2 g)
                    else Nothing

-- Should notify P2 that it started
handleStart :: Maybe GameState
handleStart = error "todo"

-- Should retry connection to P1
handleConnect :: Maybe GameState
handleConnect = error "todo"


-- Should notify P2 that it is ready
handleReady1 = error "todo"


-- Should retry checking if P1 is ready
handleReady2 = error "todo"
