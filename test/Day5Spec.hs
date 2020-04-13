module Day5Spec where

import Test.Hspec
import Day5

test_day5 :: IO ()
test_day5 = hspec $ do
  describe "runIntCode" $ do
    it "1,0,0,0,99" $ do
      runIntCode "1,0,0,0,99" `shouldBe` [2,0,0,0,99]

    it "2,3,0,3,99" $ do
      runIntCode "2,3,0,3,99" `shouldBe` [2,3,0,6,99]

    it "2,4,4,5,99,0" $ do
      runIntCode "2,4,4,5,99,0" `shouldBe` [2,4,4,5,99,9801]

    it "1,1,1,4,99,5,6,0,99" $ do
      runIntCode "1,1,1,4,99,5,6,0,99" `shouldBe` [30,1,1,4,2,5,6,0,99]

    it "Test input: 3,3,99,0" $ do
      runIntCode "3,3,99,0" `shouldBe` [3,3,99,5]

    it "Test output: 1,0,0,0,4,0,99" $ do
      runIntCode "1,0,0,0,4,0,99" `shouldBe` [2,0,0,0,4,0,99]

    it "Test direct params - website example" $ do
      runIntCode "1002,4,3,4,33" `shouldBe` [1002,4,3,4,99]

    it "Test direct params - website example 2" $ do
      runIntCode "1101,100,-1,4,0" `shouldBe` [1101,100,-1,4,99]

    it "Test JumpIfTrue - direct params" $ do
      runIntCode "1105,1,1,0,6,99,0" `shouldBe` [1105,1,1,0,6,99,1106]

    it "Test JumpIfTrue - indirect params" $ do
      runIntCode "5,2,6,0,6,99,1" `shouldBe` [5,2,6,0,6,99,5]

    it "Test JumpIfTrue - no jump" $ do
      runIntCode "1105,0,1,99" `shouldBe` [1105,0,1,99]

    it "Test JumpIfFalse - direct params" $ do
      runIntCode "1106,0,2,1,6,7,99,0" `shouldBe` [1106,0,2,1,6,7,99,0]

    it "Test JumpIfFalse - indirect params" $ do
      runIntCode "6,6,4,99,1,6,0,9,99,0" `shouldBe` [6,6,4,99,1,6,0,9,99,6]

    it "Test JumpIfFalse - no jump" $ do
      runIntCode "1006,0,1,99" `shouldBe` [1006,0,1,99]

    it "Test LessThan - a < b" $ do
      runIntCode "1107,0,1,5,99,8" `shouldBe` [1107,0,1,5,99,1]

    it "Test LessThan - a = b" $ do
      runIntCode "1107,1,1,5,99,8" `shouldBe` [1107,1,1,5,99,0]

    it "Test LessThan - a > b" $ do
      runIntCode "1107,2,1,5,99,8" `shouldBe` [1107,2,1,5,99,0]

    it "Test LessThan - a < b, indirect" $ do
      runIntCode "7,0,4,5,99,8" `shouldBe` [7,0,4,5,99,1]

    it "Test Equals - a < b" $ do
      runIntCode "1108,0,1,5,99,8" `shouldBe` [1108,0,1,5,99,0]

    it "Test Equals - a = b" $ do
      runIntCode "1108,1,1,5,99,8" `shouldBe` [1108,1,1,5,99,1]

    it "Test Equals - a > b" $ do
      runIntCode "1108,2,1,5,99,8" `shouldBe` [1108,2,1,5,99,0]

    it "Test Equals - a = b, indirect" $ do
      runIntCode "8,0,5,5,99,8" `shouldBe` [8,0,5,5,99,1]

  describe "getPointers" $ do
    it "All indirect" $ do
      getPointers 0 2 0 [1,2] `shouldBe` [1,2]

    it "All direct" $ do
      getPointers 11 2 0 [1,0] `shouldBe` [0,1]

    it "Overflow defaults to indirect" $ do
      getPointers 1 2 0 [1,0] `shouldBe` [0,0]

    it "A mixture" $ do
      getPointers 10101 6 0 [1,0,5,2,3,4] `shouldBe` [0,0,2,2,4,4]
