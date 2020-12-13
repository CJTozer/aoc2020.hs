module Day3Spec (spec) where

import Day3 (treesWithSlope, treesWithSlope')
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
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
