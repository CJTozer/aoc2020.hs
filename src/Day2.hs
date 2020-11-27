{-# LANGUAGE ScopedTypeVariables #-}

module Day2 (day2) where

import Data.List.Split
import Debug.Trace

day2 :: IO ()
day2 = do
  putStrLn "day2 start"
  contents <- readFile "data/day2"
  putStrLn "day2 end"
