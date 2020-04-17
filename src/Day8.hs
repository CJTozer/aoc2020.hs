{-# LANGUAGE ScopedTypeVariables #-}

module Day8 (day8) where

import Data.List.Split
import Debug.Trace

day8 :: IO ()
day8 = do
  putStrLn "day8 start"
  contents <- readFile "data/day8"
  putStrLn "day8 end"
