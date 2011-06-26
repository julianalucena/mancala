-- Board.hs
-- Mancala's board.

module Board(initBoard,
             getPlayerHoles,
             getOtherPlayerHoles,
             getOtherPlayer,
             removeMancalaHole,
             board2holes,
             holes2board,
             updateHole,
             sow,
             move,
             capture,
             getLastHole) where

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


-- Captures other player's seeds
capture :: Hole -> Board -> Board
capture (player, pos) b = holes2board (getOtherPlayer player) holesCapturedSeeds
  where holesCapturedSeeds = updateHole mancalaPos newMancalaSeeds holesRemovedSeeds
        newMancalaSeeds = (holesRemovedSeeds !! mancalaPos) + opositeSeeds
        mancalaPos = length holesRemovedSeeds - 1
        holesRemovedSeeds = updateHole pos 0 (board2holes (getOtherPlayer player) b)
        opositeSeeds = (getOtherPlayerHoles player b) !! pos

getLastHole :: Hole -> Board -> Hole
getLastHole (player, pos)  b = (getPlayer, newPosition `mod` (boardSize `div` 2))
  where seeds = (getPlayerHoles player b) !! pos
        getPlayer = getPlayerByPosition (player, newPosition) b
        newPosition = ((pos + seeds) `mod` (boardSize - 1))
        boardSize = length (fst b ++ snd b)


getPlayerByPosition :: Hole -> Board -> Player
getPlayerByPosition (player, pos) (s1, s2) = if pos >= halfBoardSize
                                             then getOtherPlayer player
                                             else player
  where halfBoardSize = (length (s1 ++ s2)) `div` 2

