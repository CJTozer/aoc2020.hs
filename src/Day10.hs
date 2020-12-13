{-# LANGUAGE ScopedTypeVariables #-}

module Day10 (day10) where

import Data.List (sort)
import Data.List.Split (splitWhen)

day10 :: IO ()
day10 = do
  putStrLn "day10 start"
  contents <- readFile "data/day10"
  let ints = sort . map read . lines $ contents :: [Int]
  print $ take 5 ints
  let fullSet = [0] ++ ints ++ [last ints + 3]
  let diffs = joltageDiffs fullSet
  print $ take 5 diffs
  print $ countOnes diffs * countThrees diffs
  print $ routesTo 3 ints
  print $ routesTo 5 ints
  print $ routesTo 6 ints
  print $ routesTo 7 ints
  print $ totalRoutes diffs
  putStrLn "day10 end"

joltageDiffs :: [Int] -> [Int]
joltageDiffs [] = []
joltageDiffs [_] = []
joltageDiffs (x : y : zs) = (y - x) : joltageDiffs (y : zs)

countNs :: Eq a => a -> [a] -> Int
countNs n x = length $ filter (== n) x
countOnes :: [Int] -> Int
countOnes = countNs 1
countThrees :: [Int] -> Int
countThrees = countNs 3

routesTo :: Int -> [Int] -> Int
routesTo 0 _ = 1
routesTo x _ | x < 0 = 0
routesTo x ints =
  -- Sum of the 3 possible routes here
  if x `elem` ints
    then sum . map (`routesTo` ints) $ [x - 1, x - 2, x - 3]
    else 0

totalRoutes :: [Int] -> Int
totalRoutes [] = 1
totalRoutes [_] = 1
totalRoutes ints =
  -- Gap of 3 is a total reset, just multiply those together
  product $ map totalRoutes' (splitWhen (== 3) ints)

-- Now using the diffs not the raw values
totalRoutes' :: [Int] -> Int
totalRoutes' ints = totalRoutes'' (reverse ints)

totalRoutes'' :: [Int] -> Int
totalRoutes'' [] = 1
totalRoutes'' [_] = 1
totalRoutes'' [_, _] = 2
totalRoutes'' (1 : 2 : xs) = totalRoutes'' xs + totalRoutes'' (2 : xs)
totalRoutes'' (2 : 1 : xs) = totalRoutes'' xs + totalRoutes'' (1 : xs)
totalRoutes'' (1 : 1 : 1 : xs) = totalRoutes'' xs + totalRoutes'' (1 : xs) + totalRoutes'' (1 : 1 : xs)
