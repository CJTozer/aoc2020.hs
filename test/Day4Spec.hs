module Day4Spec (spec) where

import Day4 (validByr)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "validByr" $ do
    it "Missing" $ do
      validByr "btr:123" `shouldBe` False
      validByr "btr:2001" `shouldBe` False
      validByr "btr:2001 blr:byr" `shouldBe` False

    it "Out of bounds" $ do
      validByr "byr:1919" `shouldBe` False
      validByr "nnn:1980 byr:2003" `shouldBe` False
      validByr "nnn:1980\nbyr:1919\nltr:2000" `shouldBe` False

    it "Valid" $ do
      validByr "byr:1920" `shouldBe` True
      validByr "byr:2002" `shouldBe` True
      validByr "nnn:wert byr:1980" `shouldBe` True
      validByr "nnn:wert\nbyr:1980\nfff:234234" `shouldBe` True
