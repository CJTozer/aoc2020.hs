{-# LANGUAGE ScopedTypeVariables #-}

module Day3 where

import Data.List.Split

day2 :: IO ()
day2 = do
  putStrLn "day3 start"
  contents <- readFile "data/day3"
  putStrLn "day2 end"

data Hor = Hor { x1::Int, x2::Int, y::Int }
data Ver = Ver { x::Int, y1::Int, y2::Int }
type Point = (Int, Int)

-- Inclusive between returns (v1 <= x <= v2) - or v1/2 the other way around
between :: Int -> Int -> Int -> Bool
between x v1 v2 =
  x >= min v1 v2 && x <= max v1 v2

intersection :: Hor -> Ver -> Maybe Point
intersection h v = do
  -- (x v) `between` (x1 h) (x2 h)
  let x_ok = between (x v) (x1 h) (x2 h)
  let y_ok = between (y h) (y1 v) (y2 v)
  case x_ok && y_ok of
    True -> Just ((x v), (y h))
    False -> Nothing
