## What is this?
A simple mancala game in Haskell.
## How can I play?
Just: 	`cabal install mancala`
## How to play
Player at down is A and player at top is B. You just need to simply choose the hole by typing it's index (wich is presented before ": ").
## Rules
Mancala has some rules that are applied to this game.
### Move again
If after a move the last seed is deposited on mancala hole, the player has another move to do.
### Capture
If after a move the last seed is deposited on a normal and empty hole, and the oposite hole has seeds, these seeds will be captured. In other words, these enemy's seeds will be deposited on player's mancala hole.
### Winner
The winner is the player that has more seeds on mancala hole + normal holes.
## Contributors
Juliana Lucena ([julianalucena](https://github.com/julianalucena))

Tiago Lima ([fltiago](https://github.com/fltiago))