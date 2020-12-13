module Day5Spec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Day5 ( seatToId )

spec :: Spec
spec = do
  describe "seatToId" $ do
    it "FBFBBFFRLR" $ do
      seatToId "FBFBBFFRLR" `shouldBe` 357
