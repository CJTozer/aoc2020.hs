module Day15Spec (spec) where

import Day15 (getNth')
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "getNth'" $ do
    it "0,3,6" $ do
      let input = [0, 3, 6]
      getNth' 3 input `shouldBe` 6
      getNth' 4 input `shouldBe` 0
      getNth' 5 input `shouldBe` 3
