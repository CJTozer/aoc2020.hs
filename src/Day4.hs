{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (day4) where

import Data.List.Split
import Debug.Trace

day4 :: IO ()
day4 = do
  putStrLn "day4 start"
  contents <- readFile "data/day4"
  putStrLn "day4 end"
