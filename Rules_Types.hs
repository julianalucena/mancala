-- Rules_Types.hs
-- Tests of Rules.hs

import Types
import Board
import Rules
import Test.HUnit

sampleBoard = ([3, 5, 0, 1, 3], [7, 0, 1, 1, 2])

isMancalaTest = TestList [
  TestCase $ assertEqual "The last hole is mancala"
    True (isMancala (A, 2) ([2, 5, 0], [7, 9, 1])),
  TestCase $ assertEqual "The other holes are not mancala"
    False (isMancala (A, 1) ([2, 5, 0], [7, 9, 1]))
  ]

hasMoveTest = TestList [
  TestCase $ assertEqual "Player A has moves to do"
    True (hasMove A ([0, 0, 2, 0, 8], [0, 0, 0, 0, 11])),
  TestCase $ assertEqual "Player B does NOT have moves to do"
     False (hasMove B ([0, 0, 2, 0, 8], [0, 0, 0, 0, 11]))
  ]

canCaptureTest = TestList [
  TestCase $ assertEqual "Player A can capture from hole 3"
    True (canCapture (A, 3) sampleBoard),
  TestCase $ assertEqual "Player A can NOT capture from hole 2"
    False (canCapture (A, 2) sampleBoard),
  TestCase $ assertEqual "Player B can capture from hole 3"
    True (canCapture (B, 3) sampleBoard),
  TestCase $ assertEqual "Player B can NOT capture from hole 0"
    False (canCapture (B, 0) sampleBoard)
  ]

makeMoveTest = TestList [
  TestCase $ assertEqual "Player A makes a simple move"
    ([0, 6, 0], [7, 9, 1]) (makeMove (A, 0) ([1, 5, 0], [7, 9, 1])),
  TestCase $ assertEqual "Player A makes a move and capture"
    ([0, 6, 1, 3], [7, 9, 0, 3]) (makeMove (A, 0) ([2, 5, 0, 0], [7, 9, 3, 3])),
  TestCase $ assertEqual "Player B makes a simple move"
    ([2, 5, 0], [7, 0, 2]) (makeMove (B, 1) ([1, 5, 0], [7, 2, 1])),
  TestCase $ assertEqual "Player B makes a move and capture"
    ([0, 6, 1, 0], [1, 9, 0, 7]) (makeMove (B, 2) ([2, 5, 0, 0], [0, 9, 5, 3]))
  ]

canMoveAgainTest = TestList [
  TestCase $ assertEqual "Player A can move again if the last hole is a mancala"
    True (canMoveAgain (A, 2) ([2, 5, 0], [7, 9, 1])),
  TestCase $ assertEqual "Player B can not move again if the last hole is not a mancala"
    False (canMoveAgain (B, 1) ([2, 5, 0], [7, 9, 1]))
  ]

getWinnerTest = TestList [
  TestCase $ assertEqual "Player A is the winner"
    A (getWinner ([2, 2, 7], [0, 2, 8])),
  TestCase $ assertEqual "Player B is the winner"
    B (getWinner ([0, 0, 4], [7, 2, 5])),
  TestCase $ assertEqual "Nobody won the game."
    Nobody (getWinner ([0, 1, 4], [1, 0, 4]))
  ]

main = runTestTT $ TestList [isMancalaTest, hasMoveTest, canCaptureTest,
  makeMoveTest, canMoveAgainTest, getWinnerTest]
