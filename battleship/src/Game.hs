{-# LANGUAGE InstanceSigs #-}
module Game (
  Game(..), Coordinate, Orientation, Boat, Status(..), boats,
  rows, cols,
  initGame, getBoat,
  addShip, isSetup,
  getOceanStatus, getTargetStatus,
  attack, hit, isHit, damaged, terminate, targeted,
  prop_init_empty,
  prop_attack_valid, prop_attack_plus_1, prop_attack_subset, prop_attack_prev, prop_attack_oob,
  prop_term,
  prop_ishit_hit, prop_ishit_miss,
  prop_add_valid, prop_add_p1, prop_add_replace, prop_add_same, prop_add_overlap, prop_add_oob,
  genGame, damageCond, genVal, genSet, overlaps, genInvalid, genBoats
  )
  where

import Data.Set
    ( Set,
      member,
      insert,
      empty,
      size,
      toList,
      fromList,
      isSubsetOf,
      intersection, difference )
import Data.Map (Map, empty, insert, assocs, keys, size, delete)
import Test.QuickCheck
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List ((\\))

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
isSetup g = Data.Map.size (_boats g) == length boats


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
terminate g = Data.Set.size (_hits g) == s || Data.Set.size (_damaged g) == s
  where
    s = foldr (\b a -> a + (_r b * _c b)) 0 boats


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


coords :: [Coordinate]
coords = [(x,y) | x <- [0..9], y <- [0..9]]

-- Test Properties
prop_init_empty :: Bool
prop_init_empty = let g = initGame
                  in null (_boats g) &&
                     null (_targeted g) &&
                     null (_damaged g) &&
                     null (_attacked g) &&
                     null (_hits g)

prop_attack_valid :: Property
prop_attack_valid = forAll (genSet 0 99) $ \s ->
                      forAll (suchThat genVal (overlaps False s)) $ \cd ->
                        let g = initGame
                            g' = g {_attacked = s}
                        in
                            isJust $ attack cd g'

prop_attack_subset :: Property
prop_attack_subset = forAll (genSet 0 99) $ \s ->
                      forAll (suchThat genVal (overlaps False s)) $ \cd ->
                        let g = initGame
                            g' = g {_attacked = s}
                            Just g'' = attack cd g'
                            a' = _attacked g''
                        in
                            Data.Set.isSubsetOf s a'

prop_attack_plus_1:: Property
prop_attack_plus_1 = forAll (genSet 0 99) $ \s ->
                      forAll (suchThat genVal (overlaps False s)) $ \cd ->
                        let g = initGame
                            g' = g {_attacked = s}
                            Just g'' = attack cd g'
                            a' = _attacked g''
                        in
                            Data.Set.size a' == Data.Set.size s + 1


prop_attack_prev :: Property
prop_attack_prev = forAll (genSet 1 100) $ \s ->
                       forAll (suchThat genVal (overlaps True s)) $ \cd ->
                         let g = initGame
                             g' = g {_attacked = s}
                         in
                             isNothing $ attack cd g'

prop_attack_oob :: Property
prop_attack_oob = forAll (genSet 1 100) $ \s ->
                      forAll genInvalid $ \cd ->
                        let g = initGame
                            g' = g {_attacked = s}
                        in
                            isNothing $ attack cd g'

prop_ishit_hit :: Property
prop_ishit_hit = forAll (genBoats 1 5) $ \m ->
                    forAll (suchThat genVal (overlapsBtCd 2 m)) $ \cd ->
                        let g = initGame
                            g' = g {_boats = m}
                        in
                            isHit cd g'

prop_ishit_miss :: Property
prop_ishit_miss = forAll (genBoats 1 5) $ \m ->
                      forAll (suchThat genVal (overlapsBtCd 0 m)) $ \cd ->
                          let g = initGame
                              g' = g {_boats = m}
                          in
                              not $ isHit cd g'

prop_term :: Property
prop_term = forAll gen $ \g -> terminate g
  where
    gen :: Gen Game
    gen = do
            s <- choose (0, 1) :: Gen Int
            l <- if s == 0 then genSet 17 17 else genSet 0 16
            r <- if s == 1 then genSet 17 17 else genSet 0 16
            let g = initGame
            return g {_damaged = l, _hits = r}

prop_add_valid :: Property
prop_add_valid = forAll (genBoats 0 4) $ \m ->
                    forAll (genBoat $ keys m) $ \b ->
                        forAll (suchThat genVal (overlapsBoat 0 m b)) $ \cd ->
                            let g = initGame
                                g' = g {_boats = m}
                            in
                                isJust $ addShip b cd g'

prop_add_p1 :: Property
prop_add_p1 = forAll (genBoats 0 4) $ \m ->
                 forAll (genBoat $ keys m) $ \b ->
                     forAll (suchThat genVal (overlapsBoat 0 m b)) $ \cd ->
                         let g = initGame
                             g' = g {_boats = m}
                             Just g'' = addShip b cd g'
                             m' = _boats g''
                         in
                            Data.Map.size m' == Data.Map.size m + 1

prop_add_replace :: Property
prop_add_replace = forAll (genBoats 1 1) $ \m ->
                     forAll (genBoat $ boats \\ keys m) $ \b ->
                       forAll (suchThat genVal (overlapsBoat 1 m b)) $ \cd ->
                         let g = initGame
                             g' = g {_boats = m}
                         in
                            isJust $ addShip b cd g'

prop_add_same :: Property
prop_add_same = forAll (genBoats 1 1) $ \m ->
                    forAll (genBoat $ boats \\ keys m) $ \b ->
                      forAll (suchThat genVal (overlapsBoat 1 m b)) $ \cd ->
                        let g = initGame
                            g' = g {_boats = m}
                            Just g'' = addShip b cd g'
                            m' = _boats g''
                        in
                            Data.Map.size m' == Data.Map.size m

prop_add_oob :: Property
prop_add_oob = forAll (genBoats 1 5) $ \m ->
                   forAll (genBoat []) $ \b ->
                        forAll (suchThat genVal (invalidBtCd b)) $ \cd ->
                            let g = initGame
                                g' = g {_boats = m}
                            in
                                isNothing $ addShip b cd g'

prop_add_overlap :: Property
prop_add_overlap = forAll (genBoats 1 4) $ \m ->
                      forAll (genBoat $ keys m) $ \b ->
                        forAll (suchThat genVal (overlapsBoat 2 m b)) $ \cd ->
                            let g = initGame
                                g' = g {_boats = m}
                            in
                                isNothing $ addShip b cd g'


-- Test Generators
genVal :: Gen Coordinate
genVal = do
          r <- choose (0, 9)
          c <- choose (0, 9)
          return (r, c)

genInvalid :: Gen Coordinate
genInvalid = do
              s <- choose (1, 3) :: Gen Int
              r <- if s >= 2 then return 10 else choose (0, 9)
              c <- if s == 1 || s == 3 then return 10 else choose (0, 9)
              return (r, c)

overlaps :: Bool -> Set Coordinate -> Coordinate -> Bool
overlaps True s cd = member cd s
overlaps _    s cd = not $ member cd s

genSet :: Int -> Int -> Gen (Set Coordinate)
genSet min max = do
                  sz <- choose (min, max)
                  genSet' sz

genSet' :: Int -> Gen (Set Coordinate)
genSet' sz = do
              s <- shuffle coords
              return $ fromList $ take sz s

genBoat :: [Boat] -> Gen Boat
genBoat bs = do
              b <- elements elems
              o <- choose (0, 1) :: Gen Int
              return $ fromJust $ getBoat b (o == 1)
  where
    used = map _id bs
    elems = filter (`notElem` used) [1..5]

overlapsBoat :: Int -> Map Boat Coordinate -> Boat -> Coordinate -> Bool
overlapsBoat md m bt cd@(r, c) = valid && overlaps md
  where
    valid :: Bool
    valid = isValid cd && isValid (r+_r bt-1, c+_c bt-1)

    overlaps :: Int -> Bool
    overlaps 0 = not (any overlap (assocs m))
    overlaps 1 = all overlap' (assocs m)
    overlaps _ = any overlap (assocs m)

    overlap :: (Boat, Coordinate) -> Bool
    overlap (bt', (r', c')) = let a = coords (bt, cd)
                                  b = coords (bt', (r', c'))
                                  a' = Data.Set.fromList a
                                  b' = Data.Set.fromList b
                              in
                                not (null (a' `intersection` b'))

    overlap' :: (Boat, Coordinate) -> Bool
    overlap' (bt', (r', c')) = let a = coords (bt, cd)
                                   b = coords (bt', (r', c'))
                                   a' = Data.Set.fromList a
                                   b' = Data.Set.fromList b
                               in
                                 if _id bt' == _id bt then
                                    not (null (a' `intersection` b'))
                                 else
                                    null (a' `intersection` b')

    coords :: (Boat, Coordinate) -> [Coordinate]
    coords (bt, (r, c)) = [(r + r', c + c') | r' <- [0..(_r bt - 1)], c' <- [0..(_c bt - 1)]]

overlapsBtCd :: Int -> Map Boat Coordinate -> Coordinate -> Bool
overlapsBtCd md m = overlapsBoat md m (Boat{ _id = 0, _r = 1, _c = 1 })

invalidBtCd :: Boat -> Coordinate -> Bool
invalidBtCd bt cd@(r, c) = not (isValid cd && isValid (r+_r bt-1, c+_c bt-1))

genBoats :: Int -> Int -> Gen (Map Boat Coordinate)
genBoats min max = do
             b <- choose (min, max)
             genBoats' b

genBoats' :: Int -> Gen (Map Boat Coordinate)
genBoats' 0 = return Data.Map.empty
genBoats' s = do
                m <- genBoats' (s-1)
                b <- genBoat $ keys m
                cd <- suchThat genVal (overlapsBoat 0 m b)
                return $ Data.Map.insert b cd m

genGame :: Int -> Gen Game
genGame d = do m <- genBoats' 5
               sfl <- shuffle $ concatMap coords (assocs m)
               let dmg = take d sfl
               let g = initGame
               return g {_boats = m, _damaged = fromList dmg}
  where
    coords :: (Boat, Coordinate) -> [Coordinate]
    coords (bt, (r, c)) = [(r + r', c + c') | r' <- [0..(_r bt - 1)], c' <- [0..(_c bt - 1)]]

damageCond :: Bool -> Game -> Coordinate -> Bool
damageCond b g cd = if b then 
                      elem cd (boats `difference` dmgd) 
                    else 
                      elem cd (all `difference` boats)
  where
    dmgd :: Set Coordinate
    dmgd = _damaged g

    boats :: Set Coordinate
    boats = fromList $ concatMap coords (assocs $ _boats g)

    coords :: (Boat, Coordinate) -> [Coordinate]
    coords (bt, (r, c)) = [(r + r', c + c') | r' <- [0..(_r bt - 1)], c' <- [0..(_c bt - 1)]]

    all :: Set Coordinate
    all = fromList [(x,y) | x <- [0..9], y <- [0..9]]
