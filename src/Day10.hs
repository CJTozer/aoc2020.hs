{-# LANGUAGE ScopedTypeVariables #-}

module Day10 (
  day10,
  countVisibleFrom,
  hasLineOfSight,
  directlyBetween,
  coLinear
  ) where

import Data.List
import Data.Ratio
import Debug.Trace

type Point = (Int, Int)

day10 :: IO ()
day10 = do
  putStrLn "day10 start"
  contents <- readFile "data/day10"
  putStrLn "day10 end"

countVisibleFrom :: [Point] -> Point -> Int
countVisibleFrom asteroids p = accVisible asteroids asteroids p

accVisible :: [Point] -> [Point] -> Point -> Int
accVisible [] _ _ = 0
accVisible (t:ts) blockers p = (if hasLineOfSight blockers t p then 1 else 0) + accVisible ts blockers p

hasLineOfSight :: [Point] -> Point -> Point -> Bool
hasLineOfSight [] _ _ = True
hasLineOfSight (b:bs) p1 p2 = (not $ directlyBetween b p1 p2) && hasLineOfSight bs p1 p2

directlyBetween :: Point -> Point -> Point -> Bool
directlyBetween (tx, ty) (x1, y1) (x2, y2) = and [
  -- False if t == p1 or p2
  not ((tx, ty) == (x1, y1)),
  not ((tx, ty) == (x2, y2)),
  -- t.x between p1.x and p2.x
  (between tx x1 x2),
  -- t.y between p1.y and p2.y
  (between ty y1 y2),
  -- t, p1, p2 are colinear
  (coLinear (tx, ty) (x1, y1) (x2, y2))
  ]

-- Inclusive between returns (x1 <= x <= x2) - or v1/2 the other way around
between :: Int -> Int -> Int -> Bool
between x v1 v2 =
  x >= min v1 v2 && x <= max v1 v2

coLinear :: Point -> Point -> Point -> Bool
coLinear (x1, y1) (x2, y2) (x3, y3) = do
  -- co-linear if the vectors p1-p2 and p1-p3 have the same direction (or -ve)
  -- Represent direction as x:y ratio for vector, which means we need to check for potential y=0 first
  let v1 = getDirection (x1 - x2) (y1 - y2)
  let v2 = getDirection (x1 - x3) (y1 - y3)
  v1 == v2

getDirection :: Int -> Int -> Maybe (Ratio Int)
getDirection x y = case y of
  0 -> Nothing -- Nothing represents the horizontal vector
  _ -> Just $ x % y -- Otherwise the x:y ratio represents the direction
