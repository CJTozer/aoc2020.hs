module Day4Spec where

import Test.Hspec
import Day4

test_day4 :: IO ()
test_day4 = hspec $ do
  describe "valid_byr" $ do
    it "Missing" $ do
      valid_byr "btr:123" `shouldBe` False
      valid_byr "btr:2001" `shouldBe` False
      valid_byr "btr:2001 blr:byr" `shouldBe` False

    it "Out of bounds" $ do
      valid_byr "byr:1919" `shouldBe` False
      valid_byr "nnn:1980 byr:2003" `shouldBe` False
      valid_byr "nnn:1980\nbyr:1919\nltr:2000" `shouldBe` False

    it "Valid" $ do
      valid_byr "byr:1920" `shouldBe` True
      valid_byr "byr:2002" `shouldBe` True
      valid_byr "nnn:wert byr:1980" `shouldBe` True
      valid_byr "nnn:wert\nbyr:1980\nfff:234234" `shouldBe` True
