module State (State(..), Event(..), initState) where 

import Network (Addr(..), initAddr)

-- Possible states of the application
data State 
  = A { conn :: Addr }
  | B { conn :: Addr }
  deriving (Show)

-- Transition events of the application
data Event
  = Flip 
  | Enter
  deriving (Show, Eq)


-- Constructs a new state
initState :: Bool -> String -> State
initState True p = A {conn = initAddr p}
initState _    p = B {conn = initAddr p}
