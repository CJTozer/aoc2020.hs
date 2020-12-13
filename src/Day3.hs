{-# LANGUAGE ScopedTypeVariables #-}

module Day3 (
  day3
, treesWithSlope
, treesWithSlope'
  ) where

day3 :: IO ()
day3 = do
  putStrLn "day3 start"
  contents <- readFile "data/day3"
  print $ show $ treesWithSlope (lines contents) 1
  print $ show $ treesWithSlope (lines contents) 3
  print $ show $ treesWithSlope (lines contents) 5
  print $ show $ treesWithSlope (lines contents) 7
  print $ show $ treesWithSlope' (lines contents) 2
  putStrLn "day3 end"

-- for slopes >= 1
treesWithSlope :: [String] -> Int -> Int
treesWithSlope [] _ = 0
treesWithSlope ls slope = length $ filter (== '#') s
  where s = [ row !! ((row_n * slope) `mod` length row)
            | (row_n, row) <- zip [0..] ls
            ]

-- For slopes < 1
treesWithSlope' :: [String] -> Int -> Int
treesWithSlope' [] _ = 0
treesWithSlope' ls slope = length $ filter (== '#') s
  where s = [ row !! ((row_n `div` slope) `mod` length row)
            | (row_n, row) <- zip [0..] ls
            , row_n `mod` slope == 0
            ]
