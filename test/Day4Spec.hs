module Day4Spec where

import Test.Hspec
import Day4

test_day4 :: IO ()
test_day4 = hspec $ do
  describe "allDigitsAscending" $ do
    it "0-9" $ do
      allDigitsAscending "0123456789" `shouldBe` True

    it "repeated digits" $ do
      allDigitsAscending "1122466779" `shouldBe` True

    it "9 to 0" $ do
      allDigitsAscending "1234567890123456789" `shouldBe` False

    it "descending" $ do
      allDigitsAscending "0123546789" `shouldBe` False

  describe "containsRepeated" $ do
    it "no repeats" $ do
      containsRepeated "0123456789" `shouldBe` False

    it "repeat at start" $ do
      containsRepeated "001234" `shouldBe` True

    it "repeat at end" $ do
      containsRepeated "012344" `shouldBe` True

    it "repeat in middle" $ do
      containsRepeated "012234" `shouldBe` True