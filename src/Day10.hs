{-# LANGUAGE ScopedTypeVariables #-}

module Day10 (day10) where

import Data.List.Split
import Debug.Trace

day10 :: IO ()
day10 = do
  putStrLn "day10 start"
  contents <- readFile "data/day10"
  putStrLn "day10 end"
