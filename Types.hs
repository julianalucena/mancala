-- Types.hs
-- Types used in Mancala game

module Types where

data Player = A | B | Nobody deriving (Eq, Show, Read)

type Board = ([Seed], [Seed])
type Hole = (Player, Position)
type Position = Int
type Seed = Int
