module Day10Spec where

import Data.List
import Test.Hspec
import Day10

testAllCoLinear a b c res = do
  coLinear a b c `shouldBe` res
  coLinear a c b `shouldBe` res
  coLinear b a c `shouldBe` res
  coLinear b c a `shouldBe` res
  coLinear c a b `shouldBe` res
  coLinear c b a `shouldBe` res

test_day10 :: IO ()
test_day10 = hspec $ do
  describe "coLinear" $ do
    it "y = x" $ do
      testAllCoLinear (0,0) (1,1) (2,2) True

    it "y = 2 - x" $ do
      testAllCoLinear (0,2) (1,1) (2,0) True

    it "Not on line with a matching y" $ do
      testAllCoLinear (0,0) (1,2) (2,2) False

    it "Not on line with a matching x" $ do
      testAllCoLinear (0,0) (1,2) (2,2) False

    it "In line, but non-trivial ratios" $ do
      testAllCoLinear (2,-4) (11,11) (-4,-14) True
