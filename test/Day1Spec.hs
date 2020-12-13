module Day1Spec (spec) where

import Day1 (findSum2, findSum3, sumTo2020)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "sumTo2020" $ do
    it "10 2010" $ do
      sumTo2020 10 2010 `shouldBe` True
    it "2020 2020" $ do
      sumTo2020 2020 2020 `shouldBe` False

  describe "findSum2" $ do
    it "10 2010" $ do
      findSum2 [10, 2010] `shouldBe` 20100
    it "9 10 2001 2010" $ do
      findSum2 [9, 10, 2001, 2010] `shouldBe` 20100

  describe "findSum3" $ do
    it "10 2000 10" $ do
      findSum3 [10, 2000, 10] `shouldBe` 200000
    it "9 10 2001 2010" $ do
      findSum3 [9, 10, 2001, 2010] `shouldBe` 180090