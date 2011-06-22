-- Types.hs
-- Types used in Mancala game

module Types where

data Player = A | B deriving (Eq, Show, Read)

type Board = ([Seed], [Seed])
type Hole = (Player, Position)
type Position = Int
type Seed = Int

isMancala :: Hole -> Board -> Bool
isMancala (_, p) b = length holes == p
  where holes = fst b
