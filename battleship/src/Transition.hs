module Transition(transition) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import State (State(..), Event(..))
import Network (NetworkEvent(..), Addr(..), sendEvent)
import Game
    ( Boat,
      Game,
      Coordinate,
      getBoat,
      addShip,
      isSetup,
      attack,
      hit,
      isHit,
      damaged,
      targeted,
      terminate )
import Data.Char (ord, isDigit, digitToInt)
import Data.List.Split ( splitOn )
import Data.Maybe (fromJust, isJust)


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
handleEnter _                   = return $ Nothing

-- Handle transitions when a network READY signal is received
handleReady :: State -> IO (Maybe State)
handleReady (Start c g)     = handleReady1 c g
handleReady (SetupWait c g) = return $ Just $ Waiting c g 
handleReady s               = return $ Just s -- ignore the READY signal if the state isn't ready to handle it

-- Transition P1 to Attacking + Send Ready Signal to P2
handleReady1 :: Addr -> Game -> IO (Maybe State)
handleReady1 c g = do
  liftIO $ sendEvent (port c) Network.Ready
  return $ Just $ Attacking c g "" False

-- Transition P2 to SetupWait + Send Ready Signal to P1
handleReady2 :: Addr -> Game -> IO (Maybe State)
handleReady2 c g = do
  liftIO $ sendEvent (port c) Network.Ready
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
                           liftIO $ sendEvent (port c) (Network.Attack cd)
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
  liftIO $ sendEvent (port c) e
  if terminate g'' then
    return $ Just $ Lose g''
  else 
    return $ Just $ Attacking c g'' "" False
handleAttack2 _ _              = return $ Nothing


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
handleCharacter st@(Start _ _) _     = Just st
handleCharacter st@(SetupWait _ _) _ = Just st
handleCharacter st@(Waiting _ _) _   = Just st
handleCharacter st ch                = Just st {s = s st ++ [ch]}

handleBackspace:: State -> Maybe State
handleBackspace st@(Start _ _)     = Just st
handleBackspace st@(SetupWait _ _) = Just st
handleBackspace st@(Waiting _ _)   = Just st
handleBackspace st                 = let str = s st
                                         s' = case str of 
                                           [] -> []
                                           x  -> init x
                                     in 
                                     Just st {s = s'}
