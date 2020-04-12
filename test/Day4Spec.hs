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

  describe "containsRepeated" $ do
    it "no repeats" $ do
      containsRepeated "0123456789" `shouldBe` False

    it "repeat at start" $ do
      containsRepeated "001234" `shouldBe` True

    it "repeat at end" $ do
      containsRepeated "012344" `shouldBe` True

    it "repeat in middle" $ do
      containsRepeated "012234" `shouldBe` True

  describe "containsExactlyTwoRepeats" $ do
    it "no repeats" $ do
      containsExactlyTwoRepeats "0123456789" `shouldBe` False

    it "repeat at start" $ do
      containsExactlyTwoRepeats "001234" `shouldBe` True

    it "repeat at end" $ do
      containsExactlyTwoRepeats "012344" `shouldBe` True

    it "repeat in middle" $ do
      containsExactlyTwoRepeats "012234" `shouldBe` True

    it "3 at start" $ do
      containsExactlyTwoRepeats "0001234" `shouldBe` False

    it "3 at end" $ do
      containsExactlyTwoRepeats "0123444" `shouldBe` False

    it "3 in middle" $ do
      containsExactlyTwoRepeats "0122234" `shouldBe` False

    it "3 and 2 in middle" $ do
      containsExactlyTwoRepeats "0222334" `shouldBe` True

  describe "numRepeats" $ do
    it "empty string" $ do
      numRepeats '1' "" `shouldBe` 0

    it "no repeats" $ do
      numRepeats '1' "2" `shouldBe` 0

    it "one repeat ends string" $ do
      numRepeats '1' "1" `shouldBe` 1

    it "one repeat" $ do
      numRepeats '1' "123" `shouldBe` 1

    it "two repeats" $ do
      numRepeats '1' "113" `shouldBe` 2

    it "four repeats" $ do
      numRepeats '1' "11113" `shouldBe` 4

    it "one repeat, more later which should be ignored" $ do
      numRepeats '1' "141113" `shouldBe` 1

  describe "isValid" $ do
    it "Website examples" $ do
      isValid "111111" `shouldBe` False
      isValid "223450" `shouldBe` False
      isValid "123789" `shouldBe` False
      isValid "112233" `shouldBe` True
      isValid "123444" `shouldBe` False
      isValid "111122" `shouldBe` True
      isValid "112222" `shouldBe` True
