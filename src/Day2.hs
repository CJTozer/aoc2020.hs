{-# LANGUAGE ScopedTypeVariables #-}

module Day2 (
  day2
  , isValidLine
  , isValidLine2
  , safeCheckChar
  ) where

import Text.Regex.TDFA
import Data.List.Split
import Debug.Trace

day2 :: IO ()
day2 = do
  putStrLn "day2 start"
  contents <- readFile "data/day2"
  let valids = filter isValidLine2 $ lines contents
  print $ show $ length valids
  print $ show $ isValidLine2 $ head $ lines contents
  putStrLn "day2 end"

isValidLine :: String -> Bool
isValidLine s = do
  let (before, match, after, submatches) = (s =~ "(.+)\\-(.+) (.): (.*)") :: (String, String, String, [String])
  let min = submatches !! 0
  let max = submatches !! 1
  let char = submatches !! 2
  let pass = submatches !! 3
  isValidPass min max char pass

isValidPass :: String -> String -> String -> String -> Bool
isValidPass min_s max_s (char:cs) pass = do
  let min = read min_s :: Int
  let max = read max_s :: Int
  let actual = length $ filter (== char) pass
  actual <= max && actual >= min

isValidLine2 :: String -> Bool
isValidLine2 s = do
  let (before, match, after, submatches) = (s =~ "(.+)\\-(.+) (.): (.*)") :: (String, String, String, [String])
  let min = submatches !! 0
  let max = submatches !! 1
  let char = submatches !! 2
  let pass = submatches !! 3
  isValidPass2 min max char pass

isValidPass2 :: String -> String -> String -> String -> Bool
isValidPass2 min_s max_s match pass = do
  let min = read min_s :: Int
  let max = read max_s :: Int
  let min_match = safeCheckChar match min pass
  let max_match = safeCheckChar match max pass
  min_match /= max_match

-- pos is 1-indexed
safeCheckChar :: String -> Int -> String -> Bool
safeCheckChar (match:_) pos s = do
  if pos > length s || pos < 1
    then False
    else (s !! (pos - 1)) == match
