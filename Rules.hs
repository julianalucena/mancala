-- Rules.hs
-- Mancala's rules

module Rules(isMancala,
             hasMove,
             canCapture,
             makeMove,
             canMoveAgain,
             getWinner,
             isPossibleMove) where

import Types
import Board
import Data.List(splitAt)

-- Indicates if the hole is a mancala
isMancala :: Hole -> Board -> Bool
isMancala (player, pos) b = (length holes - 1) == pos
  where holes = getPlayerHoles player b

-- Verify if specified player has moves to do
hasMove :: Player -> Board -> Bool
hasMove p b = any (/=0) holes
  where holes = removeMancalaHole (getPlayerHoles p b)

-- Verifies if a capture can be done
canCapture :: Hole -> Board -> Bool
canCapture (player, pos) b
  | seeds == 1 && opositeSeeds /= 0 = True
  | otherwise = False
  where seeds = (getPlayerHoles player b) !! pos
        opositeSeeds = (getOtherPlayerHoles player b) !! pos

-- Make a move and capture if it is possible
makeMove :: Hole -> Board -> Board
makeMove hole b
  | (fst lastHole) == (fst hole) &&
    canCapture lastHole boardAfterMove == True = capture lastHole boardAfterMove
  | otherwise = boardAfterMove
  where lastHole = getLastHole hole b
        boardAfterMove = move hole b

-- Verify if the user can move again
-- (based on last hole that received a seed after a move)
canMoveAgain :: Hole -> Board -> Bool
canMoveAgain hole b
  | isMancala hole b = True
  | otherwise = False

-- Retrieves the winner
getWinner :: Board -> Player
getWinner board
  | playerASeeds > playerBSeeds = A
  | playerASeeds < playerBSeeds = B
  | otherwise = Nobody
  where playerASeeds = foldr (+) 0 (fst board)
        playerBSeeds = foldr (+) 0 (snd board)

-- Verify if is a possible movement
isPossibleMove :: Hole -> Board -> Bool
isPossibleMove (player, pos) b
  | pos > length playerHoles - 2 = False
  | playerHoles !! pos == 0 = False
  | otherwise = True
  where playerHoles = getPlayerHoles player b
