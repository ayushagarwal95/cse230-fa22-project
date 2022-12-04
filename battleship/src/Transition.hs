module Transition (
  transition,
  prop_ch_nothing, prop_ch_valid, prop_ch_append,
  prop_bkspc_nothing, prop_bkspc_valid, prop_bkspc_trunc, prop_bkspc_empty,
  prop_wait_attack_hit, prop_wait_attack_miss, prop_wait_lose,
  prop_attackwait_wait_hit, prop_attackwait_wait_miss, prop_attackwait_win,
  prop_attacking_valid, prop_attacking_prev, prop_attacking_invalid,
  prop_setup_invalid, prop_setup_ready, prop_setup_ready2, prop_setup_invalid2
) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import State (State(..), Event(..))
import Network (NetworkEvent(..), Addr(..), sendEvent, initAddr, initTestAddr)
import Game
    ( Boat,
      Game (..),
      Coordinate,
      getBoat,
      addShip,
      isSetup,
      attack,
      hit,
      isHit,
      damaged,
      targeted,
      terminate, initGame, genGame, damageCond, genVal, overlaps, genSet, genInvalid, Orientation, genBoats)
import Data.Char (ord, isDigit, digitToInt)
import Data.List.Split ( splitOn )
import Data.Maybe (fromJust, isJust, isNothing)
import Test.QuickCheck
import GHC.Char
import Data.List (isPrefixOf)
import Test.QuickCheck.Monadic


-- Transition performs the actual transition of states of a given event
transition :: State -> Event -> IO (Maybe State)
transition st State.Ready          = handleReady st
transition st (State.Attack c)     = handleAttack2 st c
transition st State.Hit            = return $ handleHit st
transition st State.Miss           = return $ handleMiss st
transition st Enter                = handleEnter st
transition st (State.Character ch) = return $ handleCharacter st ch
transition st State.Backspace      = return $ handleBackspace st


-- Handle transitions when ENTER is pressed
handleEnter :: State -> IO (Maybe State)
handleEnter (Setup1 c g s _)    = return $ Just $ case handleBoat g s of
                                    Valid g'  -> Setup1 c g' "" False
                                    Invalid   -> Setup1 c g "" True
                                    _         -> Start c g
handleEnter (Setup2 c g s _)    = case handleBoat g s of
                                    Valid g'         -> return $ Just $ Setup2 c g' "" False
                                    Invalid          -> return $ Just $ Setup2 c g "" True
                                    Transition.Ready -> handleReady2 c g
handleEnter (SetupWait c g)     = handleReady2 c g
handleEnter (Attacking c g s _) = handleAttack1 c g s
handleEnter _                   = return Nothing

-- Handle transitions when a network READY signal is received
handleReady :: State -> IO (Maybe State)
handleReady (Start c g)     = handleReady1 c g
handleReady (SetupWait c g) = return $ Just $ Waiting c g
handleReady _               = return Nothing -- ignore the READY signal if the state isn't ready to handle it

-- Transition P1 to Attacking + Send Ready Signal to P2
handleReady1 :: Addr -> Game -> IO (Maybe State)
handleReady1 c g = do
  liftIO $ sendEvent (test c) (port c) Network.Ready
  return $ Just $ Attacking c g "" False

-- Transition P2 to SetupWait + Send Ready Signal to P1
handleReady2 :: Addr -> Game -> IO (Maybe State)
handleReady2 c g = do
  liftIO $ sendEvent (test c) (port c) Network.Ready
  return $ Just $ SetupWait c g

data SetupResult = Valid Game | Invalid | Ready;

handleBoat :: Game -> String -> SetupResult
handleBoat g s = case s of
  "r" -> if isSetup g then Transition.Ready else Invalid
  _   -> let split = splitOn ":" s
         in
            if length split == 3 then
              let b = mapBoat (head split) (split !! 1)
                  c = mapCoordinate (split !! 2)
              in
                if isJust b && isJust c then
                  maybe Invalid Valid (addShip (fromJust b) (fromJust c) g)
                else
                  Invalid
            else
              Invalid

mapBoat :: String -> String -> Maybe Boat
mapBoat b o = let n = getBoatNumber b
              in
                if isJust n then
                  case o of
                  "v" -> getBoat (fromJust n) True
                  "h" -> getBoat (fromJust n) False
                  _   -> Nothing
                else
                  Nothing

getBoatNumber :: String -> Maybe Int
getBoatNumber s = if length s == 1 && isDigit (head s) then
                    let d = digitToInt (head s)
                    in
                    if d >= 1 && d <= 5 then Just d else Nothing
                  else
                    Nothing

mapCoordinate :: String -> Maybe Coordinate
mapCoordinate st = if length st == 2 &&
                      ord (head st) >= ord 'a' &&
                      ord (head st) <= ord 'j' &&
                      ord (st !! 1) >= ord '0' &&
                      ord (st !! 1) <= ord '9' then
                     Just (ord (head st) - ord 'a', ord (st !! 1) - ord '0')
                   else
                     Nothing


-- Handle transition on an attack
handleAttack1 :: Addr -> Game -> String -> IO (Maybe State)
handleAttack1 c g s = case mapCoordinate s of
                       Just cd -> case attack cd g of
                         Just g' -> do
                           liftIO $ sendEvent (test c) (port c) (Network.Attack cd)
                           return $ Just $ AttackWait c g' cd
                         Nothing -> return $ Just $ Attacking c g "" True
                       Nothing -> return $ Just $ Attacking c g "" True

-- Handle transition on receiving attack events
handleAttack2 :: State -> Coordinate -> IO (Maybe State)
handleAttack2 (Waiting c g) cd = do
  let g' = targeted cd g
  let (g'', e) = if isHit cd (fromJust g') then
                  (fromJust $ damaged cd (fromJust g'), Network.Hit)
                else
                  (fromJust g', Network.Miss)
  liftIO $ sendEvent (test c) (port c) e
  if terminate g'' then
    return $ Just $ Lose g''
  else
    return $ Just $ Attacking c g'' "" False
handleAttack2 _ _              = return Nothing


-- Handle transition on receiving Hit events
handleHit :: State -> Maybe State
handleHit (AttackWait c g cd) = let g' = fromJust (hit cd g)
                                in if terminate g' then
                                    Just $ Win g'
                                  else
                                    Just $ Waiting c g'
handleHit _                   = Nothing


-- Handle transition on receiving Miss events
handleMiss :: State -> Maybe State
handleMiss (AttackWait c g _) = Just $ Waiting c g
handleMiss _                  = Nothing


handleCharacter :: State -> Char -> Maybe State
handleCharacter (Start _ _) _     = Nothing
handleCharacter (SetupWait _ _) _ = Nothing
handleCharacter (Waiting _ _) _   = Nothing
handleCharacter st ch             = Just st {s = s st ++ [ch]}

handleBackspace:: State -> Maybe State
handleBackspace (Start _ _)     = Nothing
handleBackspace (SetupWait _ _) = Nothing
handleBackspace (Waiting _ _)   = Nothing
handleBackspace st              = let str = s st
                                      s' = case str of
                                        [] -> []
                                        x  -> init x
                                  in
                                  Just st {s = s'}

-- Test Property
prop_ch_nothing :: Property
prop_ch_nothing = forAll genWState $ \s ->
                    forAll (arbitrary :: Gen Char) $ \c ->
                      isNothing $ handleCharacter s c

prop_ch_valid :: Property
prop_ch_valid = forAll (genIState False) $ \s ->
                  forAll (arbitrary :: Gen Char) $ \c ->
                    isJust $ handleCharacter s c

prop_ch_append :: Property
prop_ch_append = forAll (genIState False) $ \st ->
                  forAll (arbitrary :: Gen Char) $ \c ->
                    let st' = handleCharacter st c
                    in
                      s st ++ [c] == s (fromJust st')

prop_bkspc_nothing :: Property
prop_bkspc_nothing = forAll genWState $ \s ->
                         isNothing $ handleBackspace s

prop_bkspc_valid :: Property
prop_bkspc_valid = forAll (genIState False) $ \s ->
                         isJust $ handleBackspace s

prop_bkspc_trunc :: Property
prop_bkspc_trunc = forAll (genIState False) $ \st ->
                    let st' = handleBackspace st
                    in
                      isPrefixOf (s (fromJust st')) (s st)

prop_bkspc_empty :: Property
prop_bkspc_empty = forAll (genIState True) $ \st ->
                    let st' = handleBackspace st
                    in
                      null $ s (fromJust st')

prop_wait_attack_hit :: Property
prop_wait_attack_hit = monadicIO $ do
                                    g <- pick $ genGame 15
                                    cd <- pick (suchThat genVal (damageCond True g))
                                    let st = Waiting initTestAddr g
                                    st' <- run $ handleAttack2 st cd
                                    assert $ case fromJust st' of
                                      Attacking _ _ _ _ -> True
                                      _                 -> False

prop_wait_attack_miss :: Property
prop_wait_attack_miss = monadicIO $ do
                                    g <- pick $ genGame 16
                                    cd <- pick (suchThat genVal (damageCond False g))
                                    let st = Waiting initTestAddr g
                                    st' <- run $ handleAttack2 st cd
                                    assert $ case fromJust st' of
                                      Attacking _ _ _ _ -> True
                                      _                 -> False

prop_wait_lose :: Property
prop_wait_lose = monadicIO $ do
                               g <- pick $ genGame 16
                               cd <- pick (suchThat genVal (damageCond True g))
                               let st = Waiting initTestAddr g
                               st' <- run $ handleAttack2 st cd
                               assert $ case fromJust st' of
                                 Lose _ -> True
                                 _      -> False

prop_attackwait_wait_hit :: Property
prop_attackwait_wait_hit = forAll (genSet 1 15) $ \s ->
                              forAll (suchThat genVal (overlaps False s)) $ \c ->
                                  let g = initGame
                                      g' = g {_hits = s}
                                      st = AttackWait initTestAddr g' c
                                      st' = fromJust $ handleHit st
                                  in
                                    case st' of
                                      Waiting _ _ -> True
                                      _           -> False

prop_attackwait_win :: Property
prop_attackwait_win = forAll (genSet 16 16) $ \s ->
                          forAll (suchThat genVal (overlaps False s)) $ \c ->
                              let g = initGame
                                  g' = g {_hits = s}
                                  st = AttackWait initTestAddr g' c
                                  st' = fromJust $ handleHit st
                              in
                               case st' of
                                  Win _ -> True
                                  _     -> False

prop_attackwait_wait_miss :: Property
prop_attackwait_wait_miss = forAll (genSet 16 16) $ \s ->
                              forAll (suchThat genVal (overlaps False s)) $ \c ->
                                  let g = initGame
                                      g' = g {_hits = s}
                                      st = AttackWait initTestAddr g' c
                                      st' = fromJust $ handleMiss st
                                  in
                                    case st' of
                                      Waiting _ _ -> True
                                      _           -> False

prop_attacking_valid :: Property
prop_attacking_valid = monadicIO $ do
                                  s <- pick $ genSet 0 99
                                  c <- pick $ suchThat genVal (overlaps False s)
                                  let g = initGame
                                  let g' = g {_attacked = s}
                                  let st = Attacking initTestAddr g' (cdToStr c) False
                                  st' <- run $ handleEnter st
                                  assert $ case fromJust $ st' of
                                    AttackWait _ _ _ -> True
                                    _                -> False

prop_attacking_prev :: Property
prop_attacking_prev = monadicIO $ do
                                  s <- pick $ genSet 1 99
                                  c <- pick $ suchThat genVal (overlaps True s)
                                  let g = initGame
                                  let g' = g {_attacked = s}
                                  let st = Attacking initTestAddr g' (cdToStr c) False
                                  st' <- run $ handleEnter st
                                  assert $ case fromJust $ st' of
                                    Attacking _ _ _ True -> True
                                    _                    -> False

prop_attacking_invalid :: Property
prop_attacking_invalid = monadicIO $ do
                                  s <- pick $ genSet 0 0
                                  c <- pick $ oneof [do {c <- suchThat genInvalid (overlaps False s); return $ cdToStr c}, genString, return ""]
                                  let g = initGame
                                  let g' = g {_attacked = s}
                                  let st = Attacking initTestAddr g' c False
                                  st' <- run $ handleEnter st
                                  assert $ case fromJust $ st' of
                                    Attacking _ _ _ True -> True
                                    _                    -> False

prop_setup_ready :: Property
prop_setup_ready = monadicIO $ do
                                 m <- pick $ genBoats 5 5
                                 let g = initGame
                                 let st = Setup1 initTestAddr (g {_boats = m}) "r" False
                                 st' <- run $ handleEnter st
                                 assert $ case fromJust $ st' of
                                   Start _ _  -> True
                                   _          -> False

prop_setup_invalid :: Property
prop_setup_invalid = monadicIO $ do
                                  m <- pick $ genBoats 0 4
                                  let g = initGame
                                  c <- pick $ oneof [genInvalidSetup, genString, return ""]
                                  let st = Setup1 initTestAddr (g {_boats = m}) c False
                                  st' <- run $ handleEnter st
                                  assert $ case fromJust $ st' of
                                    Setup1 _ _ _ True -> True
                                    _                 -> False

prop_setup_ready2 :: Property
prop_setup_ready2 = monadicIO $ do
                                 m <- pick $ genBoats 5 5
                                 let g = initGame
                                 let st = Setup2 initTestAddr (g {_boats = m}) "r" False
                                 st' <- run $ handleEnter st
                                 assert $ case fromJust $ st' of
                                   SetupWait _ _  -> True
                                   _              -> False

prop_setup_invalid2 :: Property
prop_setup_invalid2 = monadicIO $ do
                                  m <- pick $ genBoats 0 4
                                  let g = initGame
                                  c <- pick $ oneof [genInvalidSetup, genString, return ""]
                                  let st = Setup2 initTestAddr (g {_boats = m}) c False
                                  st' <- run $ handleEnter st
                                  assert $ case fromJust $ st' of
                                    Setup2 _ _ _ True -> True
                                    _                 -> False


cdToStr :: Coordinate -> String
cdToStr (r,c) = [row, col]
  where
    row = chr (ord 'a' + r)
    col = chr (ord '0' + c)


-- Test Generators
genWState :: Gen State
genWState = do
              c <- elements [Start, SetupWait, Waiting]
              return (c initTestAddr initGame )

genIState :: Bool -> Gen State
genIState e = do
               s <- if e then return "" else genString
               b <- arbitrary :: Gen Bool
               genIState' s b

genIState' :: String -> Bool -> Gen State
genIState' s i = do
                  c <- elements [Setup1, Setup2, Attacking]
                  return (c initTestAddr initGame s i)

genString :: Gen String
genString = listOf1 $ elements ['a'..'z']

genInvalidSetup :: Gen String
genInvalidSetup = oneof [invalidBoat, invalidO, invalidCoord]
  where
    invalidBoat = do n <- choose (6,10) :: Gen Int
                     o <- elements ['h', 'v']
                     cd <- genVal
                     return $ (show n) ++ ":" ++ [o] ++ ":" ++ (cdToStr cd)
    invalidO = do n <- choose (1,5) :: Gen Int
                  o <- elements ['a', 'g']
                  cd <- genVal
                  return $ (show n) ++ ":" ++ [o] ++ ":" ++ (cdToStr cd)
    invalidCoord = do n <- choose (1,5) :: Gen Int
                      o <- elements ['h', 'v']
                      cd <- genInvalid
                      return $ (show n) ++ ":" ++ [o] ++ ":" ++ (cdToStr cd)
