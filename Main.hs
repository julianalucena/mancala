-- Main.hs
-- Initiates Mancala game

import Types
import Board
import Rules
import Data.List(elemIndex)


showBoard :: Board -> String
showBoard (s1, s2) = "   " ++ playerBBoard ++ " \n " ++ show (last(s2)) ++ " " ++
  replicate ((length s1 - 1) * 8) '-'  ++ " " ++ show (last(s1)) ++ " \n" ++ "   " ++ playerABoard
     where playerABoard = returnBoardA (take (length s1 -1) s1)
           playerBBoard = returnBoardB (take (length s1 -1) s2)

returnBoardA :: [Seed] -> String
returnBoardA as = concat [ "|" ++ show x ++  ":(" ++ (show (as !! x)) ++ ")| " | x <- [0..(length as - 1)]]

returnBoardB :: [Seed] -> String
returnBoardB as = concat [ "|" ++ show x ++  ":(" ++ (show (as !! x)) ++ ")| " | x <- (reverse [0..(length as - 1)])]

loop :: Player -> Board -> IO()
loop player board = do
  putStrLn ("Player " ++ show player ++ " turn.")
  putStrLn ("Choose from 0 to " ++ show (length (getPlayerHoles player board) - 2))
  putStrLn (showBoard board)
  position <- getLine
  let holeToMove = (player, read position :: Position)

  if isMancala holeToMove board
    then do putStrLn ("You can not move with your Mancala hole, try again.")
            loop player board
            return()
    else if not $ isPossibleMove holeToMove board
      then do putStrLn ("Irregular move, try again.")
              loop player board
              return()
    else do
      let lastHole = getLastHole holeToMove board
      let boardAfterMove = makeMove holeToMove board

      if hasMove player boardAfterMove
        then
          if canMoveAgain lastHole boardAfterMove
            then loop player boardAfterMove
            else loop (getOtherPlayer player) boardAfterMove
        else putStrLn (showBoard boardAfterMove ++ "\n" ++
              show (getWinner boardAfterMove) ++ " won!");

main = do
  loop A (initBoard 6 4)
