import Test.Hspec
import Day1

main :: IO ()
main = hspec $ do
  describe "fuelForModule" $ do
    it "returns 0 for 0 input" $ do
      fuelForModule 0 `shouldBe` 0

    it "returns 0 for 6 input" $ do
      fuelForModule 6 `shouldBe` 0

    it "returns 2 for 12 input" $ do
      fuelForModule 12 `shouldBe` 2

    it "returns 2 for 14 input" $ do
      fuelForModule 14 `shouldBe` 2

    it "returns 654 for 1969 input" $ do
      fuelForModule 1969 `shouldBe` 654

    it "returns 33583 for 100756 input" $ do
      fuelForModule 100756 `shouldBe` 33583

  describe "fuelNeeded" $ do
    it "returns 656 for [7, 14, 1969] input" $ do
      fuelNeeded [7, 14, 1969] `shouldBe` 656
