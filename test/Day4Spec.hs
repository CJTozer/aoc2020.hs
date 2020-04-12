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
