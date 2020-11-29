module Day3Spec where

import Test.Hspec
import Day3

test_day3 :: IO ()
test_day3 = hspec $ do
  describe "treesWithSlope'" $ do
    it "Full grid" $ do
      treesWithSlope' [".#", "##", "##", "##"] 2 `shouldBe` 1
      treesWithSlope' [".#", "##", "##", "##", "##"] 2 `shouldBe` 2
      treesWithSlope' [".#", "##", "##", "##", "##", "##", "##"] 2 `shouldBe` 3

    it "Empty grid" $ do
      treesWithSlope' ["..", "..", "..", ".."] 2 `shouldBe` 0
      treesWithSlope' ["..", "..", "..", "..", ".."] 2 `shouldBe` 0
      treesWithSlope' ["..", "..", "..", "..", "..", "..", ".."] 2 `shouldBe` 0

  describe "consistent between the two" $ do
    it "Test data" $ do
      contents <- readFile "data/day3"
      let d = lines contents
      treesWithSlope d 1 `shouldBe` treesWithSlope' d 1
