module Day14Spec (spec) where

import Day14 (Instruction (Mask), maskFromString)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "maskFromString" $ do
    it "X" $ do
      maskFromString "X" `shouldBe` Mask 0 0
    it "0" $ do
      maskFromString "0" `shouldBe` Mask 0 1
    it "1" $ do
      maskFromString "1" `shouldBe` Mask 1 0
    it "1X0X1" $ do
      maskFromString "1X0X1" `shouldBe` Mask 17 4
