module Day6Spec where

import Test.Hspec
import Day6

test_day6 :: IO ()
test_day6 = hspec $ do
  describe "constructMap" $ do
    it "Empty data" $ do
      constructMap "" `shouldBe` OrbitElement "COM" [] 0

    it "Single orbit" $ do
      constructMap "COM)123" `shouldBe` (
        OrbitElement "COM" [
            OrbitElement "123" [] 1
            ] 0
        )

    it "Two children" $ do
      constructMap "COM)123\nCOM)321" `shouldBe` (
        OrbitElement "COM" [
            OrbitElement "123" [] 1,
            OrbitElement "321" [] 1
            ] 0
        )

    it "Two deep" $ do
      constructMap "COM)123\n123)321" `shouldBe` (
        OrbitElement "COM" [
            OrbitElement "123" [
                OrbitElement "321" [] 2
                ] 1
            ] 0
        )

    it "Complex" $ do
      constructMap "321)444\nCOM)123\n123)321\n123)TTR" `shouldBe` (
        OrbitElement "COM" [
            OrbitElement "123" [
                OrbitElement "321" [
                    OrbitElement "444" [] 3
                    ] 2,
                OrbitElement "TTR" [] 2
                ] 1
            ] 0
        )

  describe "countOrbits" $ do
    it "No children" $ do
      countOrbits (OrbitElement "COM" [] 0) `shouldBe` 0
      countOrbits (OrbitElement "432" [] 1) `shouldBe` 1

    it "One child" $ do
      countOrbits (constructMap "COM)123") `shouldBe` 1

    it "Website example" $ do
      countOrbits (constructMap "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") `shouldBe` 42

  describe "isInstructionFor" $ do
    it "Parent match" $ do
      isInstructionFor "COM" ("COM", "123") `shouldBe` True
      isInstructionFor "123" ("123", "COM") `shouldBe` True

    it "Child match" $ do
      isInstructionFor "123" ("COM", "123") `shouldBe` False
      isInstructionFor "COM" ("123", "COM") `shouldBe` False

  describe "findElementById" $ do
    it "COM only - matches" $ do
      findElementById "COM" (constructMap "") `shouldBe` Just (OrbitElement "COM" [] 0)

    it "COM only - non-match" $ do
      findElementById "MOC" (constructMap "") `shouldBe` Nothing

    it "one orbit, match COM" $ do
      findElementById "COM" (constructMap "COM)123") `shouldBe` Just (OrbitElement "COM" [
                                                                         OrbitElement "123" [] 1
                                                                         ] 0)

    it "one orbit, match child" $ do
      findElementById "123" (constructMap "COM)123") `shouldBe` Just (OrbitElement "123" [] 1)

    it "one orbit, non-match" $ do
      findElementById "321" (constructMap "COM)123") `shouldBe` Nothing

    it "Website example, match" $ do
      findElementById "K" (constructMap "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") `shouldBe` Just (
        OrbitElement "K" [
            OrbitElement "L" [] 7
            ] 6
        )

    it "Website example, non-match" $ do
      findElementById "Z" (constructMap "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") `shouldBe` Nothing

  describe "findParentForId" $ do
    it "COM only" $ do
      findParentForId "COM" (constructMap "") `shouldBe` Nothing

    it "one orbit, match" $ do
      findParentForId "123" (constructMap "COM)123") `shouldBe` Just (OrbitElement "COM" [
                                                                         OrbitElement "123" [] 1
                                                                         ] 0)

    it "one orbit, non-match" $ do
      findParentForId "443" (constructMap "COM)123") `shouldBe` Nothing

    it "Website example, match" $ do
      findParentForId "L" (constructMap "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") `shouldBe` Just (
        OrbitElement "K" [
            OrbitElement "L" [] 7
            ] 6
        )

    it "Website example, non-match" $ do
      findParentForId "Z" (constructMap "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") `shouldBe` Nothing
