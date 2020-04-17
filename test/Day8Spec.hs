module Day8Spec where

import Test.Hspec
import Day8

test_day8 :: IO ()
test_day8 = hspec $ do
  describe "numXs" $ do
    it "12301230123" $ do
      numXs '0' "12301230123" `shouldBe` 2
      numZeros "12301230123" `shouldBe` 2
      numOnes "12301230123" `shouldBe` 3
      numTwos "12301230123" `shouldBe` 3

  describe "combineLayers" $ do
    it "u:000111222 l:012012012" $ do
      combineLayers "000111222" "012012012" `shouldBe` "000111012"
