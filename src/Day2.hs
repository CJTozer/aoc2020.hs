{-# LANGUAGE ScopedTypeVariables #-}

module Day2 where

day2 :: IO ()
day2 = do
  putStrLn "day2 start"
  contents <- readFile "data/day2a"
  let result = intCode contents
  print $ show result
  putStrLn "day2 end"

intCode :: String -> [Int]
intCode _ = [0]
