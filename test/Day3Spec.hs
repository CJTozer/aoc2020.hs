module Day3Spec where

import Test.Hspec
import Day3

test_day3 :: IO ()
test_day3 = hspec $ do
  describe "between" $ do
    it "1 3 5" $ do
      between 1 3 5 `shouldBe` False

    it "3 1 5" $ do
      between 3 1 5 `shouldBe` True

    it "-1 -1 5" $ do
      between (-1) (-1) 5 `shouldBe` True

    it "5 1 5" $ do
      between 5 1 5 `shouldBe` True

  describe "collectInstructions" $ do
    it "U2" $ do
      let state = collectInstructions (["U2"], (0, 0), ([], []))
      state `shouldBe` ([], (0, 2), ([], [Ver 0 0 2]))

    it "U2 R4" $ do
      let state = collectInstructions (["U2", "R4"], (0, 0), ([], []))
      state `shouldBe` ([], (4, 2), ([Hor 0 4 2], [Ver 0 0 2]))

    it "U2 R4 D30" $ do
      let state = collectInstructions (["U2", "R4", "D30"], (0, 0), ([], []))
      state `shouldBe` ([], (4, -28), ([Hor 0 4 2], [Ver 4 2 (-28), Ver 0 0 2]))

    it "U2 R4 D30 L6" $ do
      let state = collectInstructions (["U2", "R4", "D30", "L6"], (0, 0), ([], []))
      state `shouldBe` ([], (-2, -28), ([Hor 4 (-2) (-28), Hor 0 4 2], [Ver 4 2 (-28), Ver 0 0 2]))

  describe "intersection" $ do
    it "no intersection" $ do
      intersection (Hor 2 4 1) (Ver 1 2 4) `shouldBe` Nothing

    it "middle intersection" $ do
      intersection (Hor 2 4 1) (Ver 3 (-1) 3) `shouldBe` Just (3, 1)

    it "point to  middle intersection" $ do
      intersection (Hor 1 2 3) (Ver 1 2 4) `shouldBe` Just (1, 3)

    it "point to point intersection" $ do
      intersection (Hor 3 5 (-1)) (Ver 3 (-1) 3) `shouldBe` Just (3, (-1))
