-- Rules.hs
-- Mancala's rules

module Rules where

import Types
import Board
import Data.List(splitAt)

makeMove :: Hole -> Board -> Board
makeMove hole b
  | canCapture hole b == True = capture hole boardAfterMove
  | otherwise = boardAfterMove
  where boardAfterMove = move hole b

--canMoveAgain :: Hole -> Board -> Board
--canMoveAgain (player, pos) b = holes !!
--  where holes = board2holes player b
--        seeds = (getPlayerHoles player b) !! pos
