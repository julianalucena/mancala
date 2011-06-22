-- Types_Test.hs
-- Tests of Types.hs

module Types_Test(isMancalaTest) where

import Types
import Test.HUnit

isMancalaTest = TestList [
  TestCase $ assertEqual "The last hole is mancala"
    (isMancala (A, 3) ([2, 5, 0], [7, 9, 1])) True,
  TestCase $ assertEqual "The other holes are not mancala"
    (isMancala (A, 2) ([2, 5, 0], [7, 9, 1])) False
  ]

main = runTestTT $ TestList [isMancalaTest]
