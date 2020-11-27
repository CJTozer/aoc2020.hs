{-# LANGUAGE ScopedTypeVariables #-}

module Day1 (day1) where

import Data.List.Split
import Debug.Trace

day1 :: IO ()
day1 = do
  putStrLn "day1 start"
  contents <- readFile "data/day1"
  putStrLn "day1 end"
