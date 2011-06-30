-- Board_Test.hs
-- Tests of Board.hs

import Types
import Board
import Test.HUnit

sampleBoard = ([3, 5, 0, 1, 3], [7, 0, 1, 1, 2])
sampleHoles = [3, 5, 0, 1, 3, 7, 0, 1, 1, 2]

initBoardTest = TestList [
  TestCase $ assertEqual "Inits a board with 5 holes (including mancala hole) with 4 seeds"
    (initBoard 5 4) ([4, 4, 4, 4, 4, 0], [4, 4, 4, 4, 4, 0])
  ]

getPlayerHolesTest = TestList [
  TestCase $ assertEqual "Returns player A holes"
    [3, 5, 0, 1, 3] (getPlayerHoles A sampleBoard),
  TestCase $ assertEqual "Returns player B holes"
    [7, 0, 1, 1, 2] (getPlayerHoles B sampleBoard)
  ]

removeMancalaHoleTest = TestList [
  TestCase $ assertEqual "Removes mancala hole"
    [3, 4, 0, 1] (removeMancalaHole [3, 4, 0, 1, 7])
  ]

updateHoleTest = TestList [
  TestCase $ assertEqual "Updates player A hole 1 to 0"
    ([3, 0, 0, 1, 3] ++ snd sampleBoard) (updateHole 1 0 (board2holes A sampleBoard)),
  TestCase $ assertEqual "Updates player B hole 3 to 0"
    ([7, 0, 1, 0, 2] ++ fst sampleBoard) (updateHole 3 0 (board2holes B sampleBoard))
  ]

sowTest = TestList [
    TestCase $ assertEqual "Sows holes with 3 seeds initiating by hole 0"
      ([4, 1, 5, 7, 2, 6]) (sow 3 0 [3, 0, 4, 7, 2, 6]),
    TestCase $ assertEqual "Sows holes with 7 seeds initiating by hole 2"
      ([4, 1, 6, 9, 3, 6]) (sow 7 2 [3, 0, 4, 7, 2, 6]),
    TestCase $ assertEqual "Sows holes with 10 seeds initiating by hole 2"
      ([5, 2, 6, 9, 4, 6]) (sow 10 2 [3, 0, 4, 7, 2, 6])
  ]

moveTest = TestList [
  TestCase $ assertEqual "Player A moves from hole 3"
    ([3, 5, 0, 0, 4], snd sampleBoard) (move (A, 3) sampleBoard),
  TestCase $ assertEqual "Player A moves from hole 1"
    ([3, 0, 1, 2, 4], [8, 1, 1, 1, 2]) (move (A, 1) sampleBoard),
  TestCase $ assertEqual "Player B moves from hole 0"
    ([4, 6, 1, 1, 3], [0, 1, 2, 2, 3]) (move (B, 0) sampleBoard)
  ]

captureTest = TestList [
  TestCase $ assertEqual "Player A captures from hole 3"
    ([3, 5, 0, 1, 10], [0, 0, 1, 1, 2]) (capture (A, 3) sampleBoard),
  TestCase $ assertEqual "Player B captures from hole 0"
    ([3, 0, 0, 1, 3], [7, 0, 1, 1, 7]) (capture (B, 2) sampleBoard)
  ]

getLastHoleTest = TestList [
  TestCase $ assertEqual "Return last hole from hole 6"
    (A, 6) (getLastHole (A, 4) ([4, 0, 5, 5, 15, 5, 1], [4, 4, 4, 4, 4, 4, 0])),
  TestCase $ assertEqual "Return last hole from hoke 4"
    (B, 0) (getLastHole (A, 2) ([4, 0, 5, 5, 15, 5, 1], [4, 4, 4, 4, 4, 4, 0]))
  ]

main = runTestTT $ TestList [initBoardTest, removeMancalaHoleTest,
  getPlayerHolesTest, updateHoleTest, sowTest, captureTest,
  getLastHoleTest]

