module Day14Spec (spec) where

import Day14 (collectAddresses)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "collectAddresses" $ do
    it "0, 0" $ do
      collectAddresses "0" "0" `shouldBe` ["0"]
    it "1, 0" $ do
      collectAddresses "1" "0" `shouldBe` ["1"]
    it "0, 1" $ do
      collectAddresses "0" "1" `shouldBe` ["1"]
    it "000000000000000000000000000000101010, 000000000000000000000000000000X1001X" $ do
      collectAddresses "000000000000000000000000000000101010" "000000000000000000000000000000X1001X"
        `shouldBe` [ "000000000000000000000000000000011010"
                   , "000000000000000000000000000000011011"
                   , "000000000000000000000000000000111010"
                   , "000000000000000000000000000000111011"
                   ]