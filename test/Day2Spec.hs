module Day2Spec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Day2 ( isValidLine, isValidLine2, safeCheckChar )

spec :: Spec
spec = do
  describe "isValidLine" $ do
    it "Trivially valid" $ do
      isValidLine "0-2 a: a" `shouldBe` True

  describe "safeCheckChar" $ do
    it "char in bounds" $ do
      safeCheckChar "a" 1 "abc" `shouldBe` True
      safeCheckChar "a" 2 "abc" `shouldBe` False
      safeCheckChar "b" 2 "abc" `shouldBe` True
      safeCheckChar "d" 2 "abc" `shouldBe` False
      safeCheckChar "c" 3 "abc" `shouldBe` True

    it "char out of bounds" $ do
      safeCheckChar "a" 0 "aaa" `shouldBe` False
      safeCheckChar "a" 4 "aaa" `shouldBe` False

  describe "isValidLine2" $ do
    it "Valid passwords" $ do
      isValidLine2 "1-2 a: a" `shouldBe` True
      isValidLine2 "1-3 a: abc" `shouldBe` True
      isValidLine2 "2-3 c: abc" `shouldBe` True

    it "Invalid passwords" $ do
      isValidLine2 "1-2 a: aaa" `shouldBe` False
