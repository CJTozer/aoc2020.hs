module Day18Spec (spec) where

import Day18 (doCalc, takeUntilClosingBracket, parseExpression, evalExpression, Part(Value))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "takeUntilClosingBracket" $ do
    it "123123123" $ do
      takeUntilClosingBracket 0 "123123123" `shouldBe` ("", "123123123")
    it "123123)123" $ do
      takeUntilClosingBracket 1 "123123)123" `shouldBe` ("123123)", "123")
    it "123)" $ do
      takeUntilClosingBracket 1 "123)" `shouldBe` ("123)", "")
    it ")123" $ do
      takeUntilClosingBracket 1 ")123" `shouldBe` (")", "123")
    it "(9 * 4 * (2 + 4 * 4 + 9 + 4 * 3) + 2) + ((4 * 8) * (5 + 9 * 8 + 2) + (7 * 7 + 2 + 7)) + 4 + 6" $
      do
        takeUntilClosingBracket 1 "9 * 4 * (2 + 4 * 4 + 9 + 4 * 3) + 2) + ((4 * 8) * (5 + 9 * 8 + 2) + (7 * 7 + 2 + 7)) + 4 + 6"
        `shouldBe` ("9 * 4 * (2 + 4 * 4 + 9 + 4 * 3) + 2)", " + ((4 * 8) * (5 + 9 * 8 + 2) + (7 * 7 + 2 + 7)) + 4 + 6")

  describe "doCalc" $ do
    it "1" $ do
      doCalc "1" `shouldBe` 1
    it "1 + 1" $ do
      doCalc "1 + 1" `shouldBe` 2
    it "2 * 3 + (4 * 5)" $ do
      doCalc "2 * 3 + (4 * 5)" `shouldBe` 26
    it "5 + (8 * 3 + 9 + 3 * 4 * 3)" $ do
      doCalc "5 + (8 * 3 + 9 + 3 * 4 * 3)" `shouldBe` 437
    it "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" $ do
      doCalc "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" `shouldBe` 12240
    it "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" $ do
      doCalc "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" `shouldBe` 13632

  describe "evalExpression" $ do
    it "1" $ do
      (evalExpression . parseExpression) "1" `shouldBe` Value 1
    it "1 + 1" $ do
      (evalExpression . parseExpression) "1 + 1" `shouldBe` Value 2
    it "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" $ do
      (evalExpression . parseExpression) "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" `shouldBe` Value 23340