module Day3Spec where

import Test.Hspec
import Day3

test_day3 :: IO ()
test_day3 = hspec $ do
  describe "between" $ do
    it "1 3 5" $ do
      between 1 3 5 `shouldBe` False

    it "3 1 5" $ do
      between 3 1 5 `shouldBe` True

    it "-1 -1 5" $ do
      between (-1) (-1) 5 `shouldBe` True

    it "5 1 5" $ do
      between 5 1 5 `shouldBe` True

  describe "intersection" $ do
    it "no intersection" $ do
      intersection (Hor 2 4 1) (Ver 1 2 4) `shouldBe` Nothing
