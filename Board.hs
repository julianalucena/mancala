-- Board.hs
-- Mancala's board.

module Board(initBoard,
             getPlayerHoles,
             getOtherPlayerHoles,
             removeMancalaHole,
             hasMove,
             board2holes,
             holes2board,
             updateHole,
             sow,
             move,
             canCapture,
             capture) where

import Types
import Data.List(splitAt)

-- Inits Board with X holes (including mancala hole) with Y seeds
initBoard :: Int -> Seed -> Board
initBoard nHoles seeds  = (holes, holes)
  where holes = replicate nHoles seeds

-- Returns player's holes
getPlayerHoles :: Player -> Board -> [Seed]
getPlayerHoles A board = fst board
getPlayerHoles B board = snd board

getOtherPlayerHoles :: Player -> Board -> [Seed]
getOtherPlayerHoles player b = getPlayerHoles (getOtherPlayer player) b

getOtherPlayer :: Player -> Player
getOtherPlayer player = if player == A
                      then B
                      else A

-- Return just normal holes
removeMancalaHole :: [Seed] -> [Seed]
removeMancalaHole seeds = reverse (tail (reverse seeds))

-- Verify if specified player has moves to do
hasMove :: Player -> Board -> Bool
hasMove p b = any (/=0) holes
  where holes = removeMancalaHole (getPlayerHoles p b)

board2holes :: Player -> Board -> [Seed]
board2holes player b = if player == A
                        then fst b ++ snd b
                        else snd b ++ fst b

holes2board :: Player -> [Seed] -> Board
holes2board p holes = if p == A
                        then splitAt (length holes `div` 2) holes
                        else (snd (holes2board A holes), fst (holes2board A holes))

-- Updates seeds quantity on a hole
updateHole :: Position -> Seed -> [Seed] -> [Seed]
updateHole pos seed holes = take pos holes ++ (seed : drop (pos + 1) holes)

-- Sows X seeds on holes
sow :: Int -> Position -> [Seed] -> [Seed]
sow n pos holes =
  if n <= length toBeSown
    then take pos holes ++ sown ++ drop (pos + n) holes
    else sow (n - length toBeSown) 0
      (take pos holes ++ sown ++ drop (pos + length toBeSown) holes)
  where sown = map (+1) toBeSown
        toBeSown = take n (drop pos (removeMancalaHole holes))

-- Makes the move
move :: Hole -> Board -> Board
move (player, pos) b = holes2board player sownBoard
  where sownBoard = sow seeds (pos + 1) holesSeedsRemoved
        holesSeedsRemoved = updateHole pos 0 allHoles
        allHoles = board2holes player b
        seeds = (getPlayerHoles player b) !! pos

-- Verifies if a capture can be done
canCapture :: Hole -> Board -> Bool
canCapture (player, pos) b
  | seeds == 1 && opositeSeeds /= 0 = True
  | otherwise = False
  where seeds = (getPlayerHoles player b) !! pos
        opositeSeeds = (getOtherPlayerHoles player b) !! pos

-- Captures other player's seeds
capture :: Hole -> Board -> Board
capture (player, pos) b = holes2board (getOtherPlayer player) holesCapturedSeeds
  where holesCapturedSeeds = updateHole mancalaPos newMancalaSeeds holesRemovedSeeds
        newMancalaSeeds = (holesRemovedSeeds !! mancalaPos) + opositeSeeds
        mancalaPos = length holesRemovedSeeds - 1
        holesRemovedSeeds = updateHole pos 0 (board2holes (getOtherPlayer player) b)
        opositeSeeds = (getOtherPlayerHoles player b) !! pos

