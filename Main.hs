-- Main.hs
-- Initiates Mancala game

import Types
import Board

loop :: Board -> IO()
loop b =  do
  putStrLn (show b)
  holeToMoveStr <- getLine
  let holeToMove = read holeToMoveStr :: Hole
  let newB = move holeToMove b
  let re = canCapture holeToMove newB
  let seeds = (getPlayerHoles (fst holeToMove) newB) !! snd holeToMove
  let opositeSeeds = (getOtherPlayerHoles (fst holeToMove) newB) !! snd holeToMove
  putStrLn (show newB)
  putStrLn (show seeds)
  putStrLn (show opositeSeeds)
  putStrLn (show re)
  if re
    then
      loop (capture holeToMove newB)
    else
      loop newB

main = loop (initBoard 4 2)
