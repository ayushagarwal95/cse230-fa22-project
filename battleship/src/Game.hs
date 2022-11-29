{-# LANGUAGE InstanceSigs #-}
module Game (
  Game, Coordinate, Orientation, Boat, Status(..),
  initGame, getBoat,
  addShip, isSetup,
  getOceanStatus, getTargetStatus,
  attack, hit, isHit, damaged, terminate, targeted)
  where

import Data.Set (Set, member, insert, empty, size, toList, fromList)
import Data.Map (Map, empty, insert, assocs, keys, size, delete)

type Coordinate = (Int, Int)
type Orientation = Bool

data Game = Game
    {
        _boats     :: Map Boat Coordinate, -- Position of boats
        _targeted  :: Set Coordinate,      -- All coordinates (of own board) that have been targeted
        _damaged   :: Set Coordinate,      -- Coordinates of player's boats that were hit
        _attacked  :: Set Coordinate,      -- All coordinates that have been tried before
        _hits      :: Set Coordinate       -- All coordinates that were hits
    }
    deriving (Eq, Show)

data Status = E | H | M | B;

data Boat = Boat
    {
        _id :: Int, -- id of the boat (necessary to differentiate boat 2 and 3)
        _r  :: Int, -- row blocks of the boat
        _c  :: Int  -- col blocks of the boat
    }
    deriving (Ord, Show)

instance Eq Boat where
    (==) :: Boat -> Boat -> Bool
    (==) a b = _id a  == _id b


rows, cols :: Int
rows = 10
cols = 10

b1, b2, b3, b4, b5 :: Boat
b1 = Boat{ _id = 1, _r = 1, _c = 2 }
b2 = Boat{ _id = 2, _r = 1, _c = 3 }
b3 = Boat{ _id = 3, _r = 1, _c = 3 }
b4 = Boat{ _id = 4, _r = 1, _c = 4 }
b5 = Boat{ _id = 5, _r = 1, _c = 5 }

boats :: [Boat]
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
initGame:: Game
initGame = Game {
    _boats     = Data.Map.empty,
    _targeted  = Data.Set.empty,
    _damaged   = Data.Set.empty,
    _attacked  = Data.Set.empty,
    _hits      = Data.Set.empty
    }


-- Possibly add the ship at the position if the coordinate is valid
--   Note: Validation is performed by ensuring that the number of 
--   valid blocks matches the number of expected blocks
addShip :: Boat -> Coordinate -> Game -> Maybe Game
addShip b cd g = let b'       = insertBoat b cd (_boats g)
                     expected = foldl f 0 (keys b')
                     actual   = length (filter isValid (toList (fromList (concatMap coords (assocs b')))))
                 in
                     if expected == actual then
                         Just (g {_boats = b'})
                     else
                         Nothing
    where
        f area bt             = area + _r bt * _c bt
        coords (bt, (r, c)) = [(r + r', c + c') | r' <- [0..(_r bt - 1)], c' <- [0..(_c bt - 1)]]

-- Custom insert to get around problems when orientation is factored in
insertBoat :: Boat -> Coordinate -> Map Boat Coordinate -> Map Boat Coordinate
insertBoat b c m = let matches = filter (== b) $ keys m
                   in
                     if null matches then
                        Data.Map.insert b c m
                     else
                        Data.Map.insert b c $ Data.Map.delete (head matches) m


-- Determines if the game is considered fully setup (all boats exist on the board)
isSetup :: Game -> Bool
isSetup g = Data.Map.size (_boats g) == 5


-- Possibly attack the coordinate if valid (new coordinate)
attack :: Coordinate -> Game -> Maybe Game
attack c g = if isValid c && not (member c (_attacked g)) then
                Just g {_attacked = Data.Set.insert c (_attacked g)}
             else
                Nothing


-- Record that the coordinate was a hit if valid
hit :: Coordinate -> Game -> Maybe Game
hit c g = Just g {_hits = Data.Set.insert c (_hits g)}


-- Determine if the attack was a hit
isHit :: Coordinate -> Game -> Bool
isHit (r, c) g = (Data.Map.size (_boats g) > 0) && any f (assocs (_boats g))
  where
    f :: (Boat, Coordinate) -> Bool
    f (b, (r', c')) = r >= r' && r < _r b + r' && c >= c' && c < _c b + c'


-- Record that the coordinate was chosen by the opponent and was a hit
damaged :: Coordinate -> Game -> Maybe Game
damaged c g = Just g {_damaged = Data.Set.insert c (_damaged g)}


-- Record that the coordinate was chosen by the opponent and was a hit
targeted:: Coordinate -> Game -> Maybe Game
targeted c g = Just g {_targeted = Data.Set.insert c (_targeted g)}


-- Possible end the game if either player has won
terminate :: Game -> Bool
terminate g = Data.Set.size (_hits g) == 17 || Data.Set.size (_damaged g) == 17


-- Determine if the coordinate exists on the board
isValid :: Coordinate -> Bool
isValid (r, c) = r >= 0 && r < rows && c >= 0 && c < cols


-- Returns the metadata of a coordinate on the ocean grid
getOceanStatus :: Coordinate -> Game -> Status
getOceanStatus cd g
  | member cd $ _damaged g  = H
  | isHit cd g              = B
  | member cd $ _targeted g = M
  | otherwise               = E


-- Returns the metadata of a coordinate on the target grid
getTargetStatus:: Coordinate -> Game -> Status
getTargetStatus cd g
  | member cd $ _hits g     = H
  | member cd $ _attacked g = M
  | otherwise               = E
