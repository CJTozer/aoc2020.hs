module Day13Spec where

import Day13 (
  earliestBusAfter,
  parseTimetableForPart2,
  part2Inner,
  validPart2,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "earliestBusAfter" $ do
    it "Simple Tests" $ do
      earliestBusAfter 100 1 `shouldBe` 100
      earliestBusAfter 100 2 `shouldBe` 100
      earliestBusAfter 100 3 `shouldBe` 102
      earliestBusAfter 100 4 `shouldBe` 100
      earliestBusAfter 100 5 `shouldBe` 100
      earliestBusAfter 100 6 `shouldBe` 102
      earliestBusAfter 100 7 `shouldBe` 105

  describe "validPart2" $ do
    it "17,x,13,19 / 3417" $ do
      validPart2 3417 (parseTimetableForPart2 ["17", "x", "13", "19"]) `shouldBe` True
    it "67,7,59,61 / 754018" $ do
      validPart2 754018 (parseTimetableForPart2 ["67", "7", "59", "61"]) `shouldBe` True

  describe "part2Inner" $ do
    it "2,3" $ do
      part2Inner 1 1 (parseTimetableForPart2 ["2", "3"]) `shouldBe` 2
    it "2,x,x,3" $ do
      part2Inner 1 1 (parseTimetableForPart2 ["2", "x", "x", "3"]) `shouldBe` 6
    it "67,7,59,61" $ do
      part2Inner 1 1 (parseTimetableForPart2 ["67", "7", "59", "61"]) `shouldBe` 754018
