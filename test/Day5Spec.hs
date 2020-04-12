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
      runIntCode "3,3,99,1" `shouldBe` [3,3,99,1]

    it "Test output: 1,0,0,0,4,0,99" $ do
      runIntCode "1,0,0,0,4,0,99" `shouldBe` [2,0,0,0,4,0,99]

    it "Test direct params - website example" $ do
      runIntCode "1002,4,3,4,33" `shouldBe` [1002,4,3,4,99]

    it "Test direct params - website example 2" $ do
      runIntCode "1101,100,-1,4,0" `shouldBe` [1101,100,-1,4,99]

  describe "getPointers" $ do
    it "All indirect" $ do
      getPointers 0 2 0 [1,2] `shouldBe` [1,2]

    it "All direct" $ do
      getPointers 11 2 0 [1,0] `shouldBe` [0,1]

    it "Overflow defaults to indirect" $ do
      getPointers 1 2 0 [1,0] `shouldBe` [0,0]

    it "A mixture" $ do
      getPointers 10101 6 0 [1,0,5,2,3,4] `shouldBe` [0,0,2,2,4,4]
