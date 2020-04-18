{-# LANGUAGE ScopedTypeVariables #-}

module Day9 (day9) where

import Data.List.Split
import Debug.Trace

import IntCode

day9 :: IO ()
day9 = do
  putStrLn "day9 start"
  program <- readFile "data/day9"
  let result = runIntCodeWithInputs program [1]
  print $ show $ getOutputs result
  putStrLn "day9 end"
