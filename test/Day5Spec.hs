module Day5Spec (spec) where

import Day5 (seatToId)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "seatToId" $ do
    it "FBFBBFFRLR" $ do
      seatToId "FBFBBFFRLR" `shouldBe` 357
