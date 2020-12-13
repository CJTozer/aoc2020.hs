{-# LANGUAGE ScopedTypeVariables #-}

module Day1 (
  day1
, sumTo2020
, findSum2
, findSum3
) where

import qualified Data.Set as Set

day1 :: IO ()
day1 = do
  putStrLn "day1 start"
  contents <- readFile "data/day1"
  let ls = lines contents
  let ns :: [Int] = map read ls
  print $ findSum3 ns
  putStrLn "day2 end"

sumTo2020 :: Int -> Int -> Bool
sumTo2020 x y = (x + y) == 2020

findSum2 :: [Int] -> Int
findSum2 ns = do
  let s = Set.fromList ns
  findSum2_set s ns 2020

findSum2_set :: Set.Set Int -> [Int] -> Int -> Int
findSum2_set _ [] _ = 0
findSum2_set s (n:ns) x =
  if Set.member (x - n) s
  then n * (x - n)
  else findSum2_set s ns x

findSum3 :: [Int] -> Int
findSum3 ns = do
  let s = Set.fromList ns
  findSum3_set s ns 2020

findSum3_set :: Set.Set Int -> [Int] -> Int -> Int
findSum3_set _ [] _ = 0
findSum3_set s (n:ns) x = do
  let y = findSum2_set s ns (x - n)
  if y == 0
  then findSum3_set s ns x
  else n * y
