module Day12Spec (spec) where

import Day12 (runInstructions)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "runInstructions" $ do
    it "Example" $ do
      runInstructions (0, 0) (1, 0) ["F10"] `shouldBe` ((10, 0), (1, 0))
      runInstructions (0, 0) (1, 0) ["F10", "N3"] `shouldBe` ((10, 3), (1, 0))
      runInstructions (0, 0) (1, 0) ["F10", "N3", "F7"] `shouldBe` ((17, 3), (1, 0))
      runInstructions (0, 0) (1, 0) ["F10", "N3", "F7", "R90"] `shouldBe` ((17, 3), (0, -1))
      runInstructions (0, 0) (1, 0) ["F10", "N3", "F7", "R90", "F11"] `shouldBe` ((17, -8), (0, -1))
