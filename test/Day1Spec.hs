module Day1Spec where

import Test.Hspec
import Day1

test_day1 :: IO ()
test_day1 = hspec $ do
  describe "instructionsToBasement" $ do
    it ") -> 1" $ do
      instructionsToBasement 0 ")" `shouldBe` 1
    it "()) -> 3" $ do
      instructionsToBasement 0 "())" `shouldBe` 3
