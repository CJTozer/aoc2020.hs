module Day5Spec where

import Test.Hspec
import Day5

test_day5 :: IO ()
test_day5 = hspec $ do
  describe "seatToId" $ do
    it "FBFBBFFRLR" $ do
      seatToId "FBFBBFFRLR" `shouldBe` 357
