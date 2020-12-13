module Day7Spec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Day7
    ( allPossibleParents,
      bagsRequired,
      lineToRule,
      mayContain,
      parseRules,
      Rule(Rule) )
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "lineToRule" $ do
    it "First two lines of input" $ do
      lineToRule "pale turquoise bags contain 3 muted cyan bags, 5 striped teal bags." `shouldBe`
        Rule "pale turquoise" [("muted cyan", 3), ("striped teal", 5)]
      lineToRule "light tan bags contain 5 posh tomato bags." `shouldBe`
        Rule "light tan" [("posh tomato", 5)]
    it "Bug #1?" $ do
      lineToRule "bright blue bags contain 1 pale beige bag, 1 light teal bag." `shouldBe`
        Rule "bright blue" [("pale beige", 1), ("light teal", 1)]

  describe "mayContain" $ do
    it "First line of input" $ do
      let r = lineToRule "pale turquoise bags contain 3 muted cyan bags, 5 striped teal bags."
      mayContain r "muted cyan" `shouldBe` True
      mayContain r "striped teal" `shouldBe` True
      mayContain r "red" `shouldBe` False
    it "Second line of input" $ do
      let r = lineToRule "light tan bags contain 5 posh tomato bags."
      mayContain r "muted cyan" `shouldBe` False
      mayContain r "striped teal" `shouldBe` False
      mayContain r "posh tomato" `shouldBe` True

  describe "allPossibleParents" $ do
    it "3 deep" $ do
      let input = "A bags contain 3 B bags.\nB bags contain 2 C bags, 5 D bags.\nC bags contain 4 E bags, 7 F bags."
      let rules = parseRules input
      allPossibleParents "A" rules `shouldBe` Set.empty
      allPossibleParents "B" rules `shouldBe` Set.fromList ["A"]
      allPossibleParents "C" rules `shouldBe` Set.fromList ["A", "B"]
      allPossibleParents "D" rules `shouldBe` Set.fromList ["A", "B"]
      allPossibleParents "E" rules `shouldBe` Set.fromList ["A", "B", "C"]
      allPossibleParents "F" rules `shouldBe` Set.fromList ["A", "B", "C"]

  describe "bagsRequired" $ do
    it "3 deep" $ do
      let input = "A bags contain 3 B bags.\nB bags contain 2 C bags, 5 D bags.\nC bags contain 4 E bags, 7 F bags.\nD bags contain no other bags.\nE bags contain no other bags.\nF bags contain no other bags."
      let rules = parseRules input
      bagsRequired "F" rules `shouldBe` 1
      bagsRequired "E" rules `shouldBe` 1
      bagsRequired "D" rules `shouldBe` 1
      bagsRequired "C" rules `shouldBe` 12
      bagsRequired "B" rules `shouldBe` 30
      bagsRequired "A" rules `shouldBe` 91


