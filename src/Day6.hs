{-# LANGUAGE ScopedTypeVariables #-}

module Day6 (day6) where

import Data.List.Split (splitOn)
import qualified Data.Set as Set

day6 :: IO ()
day6 = do
  putStrLn "day6 start"
  contents <- readFile "data/day6"
  let entries = splitOn "\n\n" contents
  putStrLn $ head entries
  print $ show $ sumGroupCounts entries
  print $ show $ sumGroupCounts' entries
  putStrLn "day6 end"

setFromLine :: String -> Set.Set Char
setFromLine = Set.fromList

setFromGroup :: String -> Set.Set Char
setFromGroup g = Set.unions $ map setFromLine $ lines g

sumGroupCounts :: [String] -> Int
sumGroupCounts gs = sum $ map (Set.size . setFromGroup) gs

setFromGroup' :: String -> Set.Set Char
setFromGroup' g = do
  let lineSets :: [Set.Set Char] = map setFromLine $ lines g
  intersectAllSets lineSets

intersectAllSets :: Ord a => [Set.Set a] -> Set.Set a
intersectAllSets [x] = x
intersectAllSets (x : xs) = Set.intersection x $ intersectAllSets xs

sumGroupCounts' :: [String] -> Int
sumGroupCounts' gs = sum $ map (Set.size . setFromGroup') gs
