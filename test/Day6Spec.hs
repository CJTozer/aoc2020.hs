module Day6Spec where

import Test.Hspec
import Day6

test_day6 :: IO ()
test_day6 = hspec $ do
  describe "constructMap" $ do
    it "Empty data" $ do
      constructMap "" `shouldBe` OrbitElement "COM" []

    it "Single orbit" $ do
      constructMap "COM)123" `shouldBe` (
        OrbitElement "COM" [
            OrbitElement "123" []
            ]
        )

    it "Two children" $ do
      constructMap "COM)123\nCOM)321" `shouldBe` (
        OrbitElement "COM" [
            OrbitElement "123" [],
            OrbitElement "321" []
            ]
        )

    it "Two deep" $ do
      constructMap "COM)123\n123)321" `shouldBe` (
        OrbitElement "COM" [
            OrbitElement "123" [
                OrbitElement "321" []
                ]
            ]
        )

    it "Complex" $ do
      constructMap "321)444\nCOM)123\n123)321\n123)TTR" `shouldBe` (
        OrbitElement "COM" [
            OrbitElement "123" [
                OrbitElement "321" [
                    OrbitElement "444" []
                    ],
                OrbitElement "TTR" []
                ]
            ]
        )

  describe "isInstructionFor" $ do
    it "Parent match" $ do
      isInstructionFor "COM" ("COM", "123") `shouldBe` True
      isInstructionFor "123" ("123", "COM") `shouldBe` True

    it "Child match" $ do
      isInstructionFor "123" ("COM", "123") `shouldBe` False
      isInstructionFor "COM" ("123", "COM") `shouldBe` False
