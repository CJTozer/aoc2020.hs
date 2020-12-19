module Day19Spec (spec) where

import Data.List.Split (splitOn)
import Day19 (regexFromRule)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "regexFromRules" $ do
    it "Full data set" $ do
      contents <- readFile "data/day19"
      let (rules : messages : _) = splitOn [""] . lines $ contents
      regexFromRule "90" rules `shouldBe` "b"
      regexFromRule "90 | 53" rules `shouldBe` "(b|a)"
      regexFromRule "50" rules `shouldBe` "ab"
      regexFromRule "65" rules `shouldBe` "(a|b)"
      regexFromRule "110" rules `shouldBe` "(a(a|b)|bb)"