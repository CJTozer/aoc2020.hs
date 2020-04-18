{-# LANGUAGE ScopedTypeVariables #-}

module Day10 (
  day10,
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

hasLineOfSight :: [Point] -> Point -> Point -> Bool
hasLineOfSight [] _ _ = True
hasLineOfSight (b:bs) p1 p2 = (not $ directlyBetween b p1 p2) && hasLineOfSight bs p1 p2

directlyBetween :: Point -> Point -> Point -> Bool
directlyBetween t p1 p2 = do
  -- False if it == p1 or p2
  -- t.x between p1.x and p2.x
  -- t.y between p1.y and p2.y
  -- t, p1, p2 are colinear
  False

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
