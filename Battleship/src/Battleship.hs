module Battleship where
import Data.Char (ord)
import Data.Set (Set, member, insert, empty, size, toList, fromList)
import Data.Map (Map, empty, insert, assocs, keys)

type Coordinate = (Int, Int)
type Orientation = Bool

data Game = Game 
    {
        _boats     :: Map Boat Coordinate, -- Position of boats
        _damaged   :: Set Coordinate,      -- Coordinates of player's boats that were hit
        _attacked  :: Set Coordinate,      -- All coordinates that have been tried before
        _hits      :: Set Coordinate       -- All coordinates that were hits
    }
    deriving (Eq, Show)

data Boat = Boat 
    {
        _id :: Int, -- id of the boat (necessary to differentiate boat 2 and 3)
        _r  :: Int, -- row blocks of the boat
        _c  :: Int  -- col blocks of the boat
    }
    deriving (Ord, Show)

instance Eq Boat where 
    (==) a b = _id a  == _id b


rows = 10
cols = 10

b1 = Boat{ _id = 1, _r = 1, _c = 2 }
b2 = Boat{ _id = 2, _r = 1, _c = 3 }
b3 = Boat{ _id = 3, _r = 1, _c = 3 }
b4 = Boat{ _id = 4, _r = 1, _c = 4 }
b5 = Boat{ _id = 5, _r = 1, _c = 5 }

boats = [b1, b2, b3, b4, b5]

-- Mapper function of Boat # and orientation to type of boat
getBoat :: Int -> Orientation -> Maybe Boat 
getBoat n o = if n > 0 && n <= length boats then
                  let b = (boats !! (n-1))
                      f = b {_r = _c b, _c = _r b}
                  in 
                  if not o then Just b else Just f
              else 
                  Nothing  

-- Initialize a new empty game
empty :: Game
empty = Game {
    _boats     = Data.Map.empty,
    _damaged   = Data.Set.empty, 
    _attacked  = Data.Set.empty, 
    _hits      = Data.Set.empty 
    }

-- Initialize a new empty game
initGame :: IO Game
initGame = return Battleship.empty

-- Possibly add the ship at the position if the coordinate is valid
--   Note: Validation is performed by ensuring that the number of 
--   valid blocks matches the number of expected blocks
addShip :: Boat -> Coordinate -> Game -> Maybe Game
addShip b c g = let b'       = Data.Map.insert b c (_boats g)
                    expected = foldl e 0 (keys b')
                    actual   = length (filter isValid (toList (fromList (concatMap coords (assocs b')))))
                in 
                    if expected == actual then
                        Just (g {_boats = b'})
                    else 
                        Nothing
    where
        e b bt             = b + _r bt * _c bt
        coords (b, (r, c)) = [(r + r', c + c') | r' <- [0..(_r b - 1)], c' <- [0..(_c b - 1)]]


-- Possibly attack the coordinate if valid (new coordinate)
attack :: Coordinate -> Game -> Maybe Game
attack c g = if isValid c && not (member c (_attacked g)) then 
                Just g {_attacked = Data.Set.insert c (_attacked g)}
             else 
                Nothing


-- Record that the coordinate was a hit if valid
hit :: Coordinate -> Game -> Game
hit c g = g {_hits = Data.Set.insert c (_hits g)}


-- Determine if the attack was a hit
isHit :: Coordinate -> Game -> Bool
isHit (r, c) g = not (any f (assocs (_boats g)))
  where 
    f :: (Boat, Coordinate) -> Bool
    f (b, (r', c')) = r >= r' && r < _r b + r' && c >= c' && c < _c b + c'


-- Record that the coordinate was chosen by the opponent
damaged :: Coordinate -> Game -> Game
damaged c g = g {_damaged = Data.Set.insert c (_damaged g)}


-- Possible end the game if either player has won
terminate :: Game -> Bool
terminate g = size (_hits g) == 17 || size (_damaged g) == 17


-- Determine if the coordinate exists on the board
isValid :: Coordinate -> Bool
isValid (r, c) = r >= 0 && r < rows && c >= 0 && c < cols 
