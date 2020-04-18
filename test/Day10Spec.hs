module Day10Spec where

import Data.List
import Test.Hspec
import Day10

testAllCoLinear a b c res = do
  coLinear a b c `shouldBe` res
  coLinear a c b `shouldBe` res
  coLinear b a c `shouldBe` res
  coLinear b c a `shouldBe` res
  coLinear c a b `shouldBe` res
  coLinear c b a `shouldBe` res

test_day10 :: IO ()
test_day10 = hspec $ do
  describe "coLinear" $ do
    it "y = x" $ do
      testAllCoLinear (0,0) (1,1) (2,2) True

    it "y = 2 - x" $ do
      testAllCoLinear (0,2) (1,1) (2,0) True

    it "Not on line with a matching y" $ do
      testAllCoLinear (0,0) (1,2) (2,2) False

    it "Not on line with a matching x" $ do
      testAllCoLinear (0,0) (1,2) (2,2) False

    it "In line, but non-trivial ratios" $ do
      testAllCoLinear (2,-4) (11,11) (-4,-14) True

  describe "directlyBetween" $ do
    it "On line" $ do
      directlyBetween (1,1) (0,0) (2,2) `shouldBe` True
      directlyBetween (1,1) (2,2) (0,0) `shouldBe` True
      directlyBetween (0,0) (0,0) (2,2) `shouldBe` False
      directlyBetween (2,2) (0,0) (2,2) `shouldBe` False
      directlyBetween (3,3) (0,0) (2,2) `shouldBe` False
      directlyBetween (0,0) (1,1) (2,2) `shouldBe` False

    it "Vertical line repro" $ do
      directlyBetween (1,1) (1,0) (1,2) `shouldBe` True
      directlyBetween (1,1) (1,2) (1,0) `shouldBe` True

    it "Not on line" $ do
      directlyBetween (1,2) (0,0) (3,3) `shouldBe` False
      directlyBetween (0,3) (0,0) (3,3) `shouldBe` False
      directlyBetween (3,0) (0,0) (3,3) `shouldBe` False

  describe "hasLineOfSight" $ do
    let a = (0,0)
    let b = (1,1)
    let c = (2,2)
    let d = (1,2)

    it "2 points only" $ do
      hasLineOfSight [a, b] a b `shouldBe` True

    it "b blocks C but not the others" $ do
      let pts = [a, b, c, d]
      hasLineOfSight pts a b `shouldBe` True
      hasLineOfSight pts a c `shouldBe` False
      hasLineOfSight pts a d `shouldBe` True
      hasLineOfSight pts b a `shouldBe` True
      hasLineOfSight pts b c `shouldBe` True
      hasLineOfSight pts b d `shouldBe` True
      hasLineOfSight pts c a `shouldBe` False
      hasLineOfSight pts c b `shouldBe` True
      hasLineOfSight pts c d `shouldBe` True
      hasLineOfSight pts d a `shouldBe` True
      hasLineOfSight pts d c `shouldBe` True
      hasLineOfSight pts d d `shouldBe` True

    it "Repro for countVisibleFrom bug: (1,1) should block (1,0) from seeing (1,2)" $ do
      hasLineOfSight [b, d] (1, 0) d `shouldBe` False

  describe "countVisibleFrom" $ do
    it "Trivial field - single asteroid" $ do
      let asteroids = [(0,0)]
      countVisibleFrom asteroids (1, 0) `shouldBe` 1
      countVisibleFrom asteroids (1, 1) `shouldBe` 1
      countVisibleFrom asteroids (0, 1) `shouldBe` 1

    it "Small Field" $ do
      let asteroids = [(0,0), (1,1), (0, 2), (1,2), (2,2), (3,3)]
      countVisibleFrom asteroids (1, 0) `shouldBe` 5
      countVisibleFrom asteroids (2, 0) `shouldBe` 5
      countVisibleFrom asteroids (3, 0) `shouldBe` 6
      countVisibleFrom asteroids (0, 1) `shouldBe` 6
      countVisibleFrom asteroids (2, 1) `shouldBe` 6
      countVisibleFrom asteroids (3, 1) `shouldBe` 6
      countVisibleFrom asteroids (3, 2) `shouldBe` 4
      countVisibleFrom asteroids (0, 3) `shouldBe` 5
      countVisibleFrom asteroids (1, 3) `shouldBe` 5
      countVisibleFrom asteroids (2, 3) `shouldBe` 6
      countVisibleFrom asteroids (2, 4) `shouldBe` 5
      countVisibleFrom asteroids (4, 4) `shouldBe` 3
