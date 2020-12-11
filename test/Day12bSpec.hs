module Day12bSpec where

import Test.Hspec
import Day12b

test_day12b :: IO ()
test_day12b = hspec $ do
  describe "runInstructions" $ do
    it "Example" $ do
      runInstructions (0, 0) (10, 1) ["F10"] `shouldBe` ((100, 10), (10, 1))
      runInstructions (0, 0) (10, 1) ["F10", "N3"] `shouldBe` ((100, 10), (10, 4))
      runInstructions (0, 0) (10, 1) ["F10", "N3", "F7"] `shouldBe` ((170, 38), (10, 4))
      runInstructions (0, 0) (10, 1) ["F10", "N3", "F7", "R90"] `shouldBe` ((170, 38), (4, -10))
      runInstructions (0, 0) (10, 1) ["F10", "N3", "F7", "R90", "F11"] `shouldBe` ((214, -72), (4, -10))
