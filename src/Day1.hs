{-# LANGUAGE ScopedTypeVariables #-}

module Day1 (
  day1,
  instructionsToBasement
  ) where

import Data.List.Split
import Debug.Trace

day1 :: IO ()
day1 = do
  putStrLn "day1 start"
  contents <- readFile "data/day1"
  print $ show $ numTimesFound '(' contents
  print $ show $ numTimesFound ')' contents
  print $ show $ instructionsToBasement 0 contents
  putStrLn "day1 end"

numTimesFound :: Eq a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (== x)) xs

instructionsToBasement :: Int -> String -> Int
instructionsToBasement (-1) _ = 0
instructionsToBasement f ('(':xs) = 1 + instructionsToBasement (f + 1) xs
instructionsToBasement f (')':xs) = 1 + instructionsToBasement (f - 1) xs
