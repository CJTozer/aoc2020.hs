module Day2Spec where

import Test.Hspec
import Day2

test_day2 :: IO ()
test_day2 = hspec $ do
  describe "intCode" $ do
    it "1,0,0,0,99" $ do
      intCode "1,0,0,0,99" `shouldBe` [2,0,0,0,99]

    it "2,3,0,3,99" $ do
      intCode "2,3,0,3,99" `shouldBe` [2,3,0,6,99]

    it "2,4,4,5,99,0" $ do
      intCode "2,4,4,5,99,0" `shouldBe` [2,4,4,5,99,9801]

    it "1,1,1,4,99,5,6,0,99" $ do
      intCode "1,1,1,4,99,5,6,0,99" `shouldBe` [30,1,1,4,2,5,6,0,99]
