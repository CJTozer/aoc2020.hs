{-# LANGUAGE ScopedTypeVariables #-}

module Day2 (
  day2
  , isValidLine
  , isValidLine2
  , safeCheckChar
  ) where

import Text.Regex.TDFA ( (=~) )

day2 :: IO ()
day2 = do
  putStrLn "day2 start"
  contents <- readFile "data/day2"
  let valids = filter isValidLine2 $ lines contents
  print $ show $ length valids
  print $ show $ isValidLine2 $ head $ lines contents
  putStrLn "day2 end"

isValidLine :: String -> Bool
isValidLine = isValidLine' isValidPass

isValidPass :: String -> String -> String -> String -> Bool
isValidPass min_s max_s (char:_) pass = do
  let min = read min_s :: Int
  let max = read max_s :: Int
  let actual = length $ filter (== char) pass
  actual <= max && actual >= min

isValidLine2 :: String -> Bool
isValidLine2 = isValidLine' isValidPass2

isValidLine' :: (String -> String -> String-> String -> Bool) -> String -> Bool
isValidLine' f s = do
  let (_, _, _, submatches) = (s =~ "(.+)\\-(.+) (.): (.*)") :: (String, String, String, [String])
  let (min:max:char:pass:_) = submatches
  f min max char pass

isValidPass2 :: String -> String -> String -> String -> Bool
isValidPass2 min_s max_s match pass = do
  let min = read min_s :: Int
  let max = read max_s :: Int
  let min_match = safeCheckChar match min pass
  let max_match = safeCheckChar match max pass
  min_match /= max_match

-- pos is 1-indexed
safeCheckChar :: String -> Int -> String -> Bool
safeCheckChar (match:_) pos s =
  not (pos > length s || pos < 1) &&
  ((s !! (pos - 1)) == match)
