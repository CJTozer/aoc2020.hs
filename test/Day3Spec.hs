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

  describe "parseWire" $ do
    it "U2,R4,D30,L6" $ do
      parseWire "U2,R4,D30,L6" `shouldBe` ([Hor 4 (-2) (-28), Hor 0 4 2], [Ver 4 2 (-28), Ver 0 0 2])

  describe "allIntersections" $ do
    it "U2,R4,D30,L6 / R6,D3,L4,U6" $ do
      let (h1, v1) = parseWire "U2,R4,D30,L6"
      let (h2, v2) = parseWire "R6,D3,L4,U6"
      allIntersections h1 v2 `shouldBe` [(2,2)]
      allIntersections h2 v1 `shouldBe` [(4,-3),(4,0),(0,0)]

  describe "closestIntersection" $ do
    it "U2,R4,D30,L6 / R6,D3,L4,U6" $ do
      let w1 = parseWire "U2,R4,D30,L6"
      let w2 = parseWire "R6,D3,L4,U6"
      closestIntersection w1 w2 `shouldBe` 4

    it "Day 3 Example 1" $ do
      closestIntersection (parseWire "R75,D30,R83,U83,L12,D49,R71,U7,L72") (parseWire "U62,R66,U55,R34,D71,R55,D58,R83") `shouldBe` 159

    it "Day 3 Example 2" $ do
      closestIntersection (parseWire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51") (parseWire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") `shouldBe` 135
