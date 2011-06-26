-- Main.hs
-- Initiates Mancala game

import Types
import Board
import Rules

loop :: Player -> Board -> IO()
loop player board = do
  putStrLn ("Player " ++ show player ++ " turn.")
  putStrLn ("Choose from 0 to " ++ show (length (getPlayerHoles player board) - 2))
  putStrLn (show board)
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
        else putStrLn (show boardAfterMove ++ "\n" ++
              show (getWinner boardAfterMove) ++ " won!");

main = do
  loop A (initBoard 4 3)
