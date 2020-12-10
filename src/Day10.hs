{-# LANGUAGE ScopedTypeVariables #-}

module Day10 (day10) where

import Data.List
import Data.List.Split
import Debug.Trace

day10 :: IO ()
day10 = do
  putStrLn "day10 start"
  contents <- readFile "data/day10"
  let ints = sort . (map read) . lines $ contents :: [Int]
  putStrLn . show $ take 5 ints
  let fullSet = [0] ++ ints ++ [(last ints) + 3]
  let diffs = joltageDiffs fullSet
  putStrLn . show $ take 5 diffs
  putStrLn . show $ (countOnes diffs) * (countThrees diffs)
  putStrLn . show $ routesTo 3 ints
  putStrLn . show $ routesTo 5 ints
  putStrLn . show $ routesTo 6 ints
  putStrLn . show $ routesTo 7 ints
  putStrLn . show $ totalRoutes diffs
  putStrLn "day10 end"


joltageDiffs :: [Int] -> [Int]
joltageDiffs [] = []
joltageDiffs [_] = []
joltageDiffs (x:y:zs) = (y - x):(joltageDiffs (y:zs))

countNs n x = length $ filter (==n) x
countOnes = countNs 1
countThrees = countNs 3

routesTo :: Int -> [Int] -> Int
routesTo 0 _ = 1
routesTo x ints | x < 0 = 0
routesTo x ints =
  if elem x ints
  then
    -- Sum of the 3 possible routes here
    routesTo (x - 1) ints +
    routesTo (x - 2) ints +
    routesTo (x - 3) ints
  else 0

totalRoutes :: [Int] -> Int
totalRoutes [] = 1
totalRoutes [x] = 1
totalRoutes ints =
  -- Gap of 3 is a total reset, just multiply those together
  product $ map totalRoutes' (splitWhen (==3) ints)

-- Now using the diffs not the raw values
totalRoutes' :: [Int] -> Int
totalRoutes' ints = totalRoutes'' (reverse ints)

totalRoutes'' :: [Int] -> Int
totalRoutes'' [] = 1
totalRoutes'' [_] = 1
totalRoutes'' [_, _] = 2
totalRoutes'' (1:2:xs) = (totalRoutes'' xs) + (totalRoutes'' (2:xs))
totalRoutes'' (2:1:xs) = (totalRoutes'' xs) + (totalRoutes'' (1:xs))
totalRoutes'' (1:1:1:xs) = (totalRoutes'' xs) + (totalRoutes'' (1:xs)) + (totalRoutes'' (1:1:xs))
