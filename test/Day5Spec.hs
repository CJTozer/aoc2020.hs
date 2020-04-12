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
