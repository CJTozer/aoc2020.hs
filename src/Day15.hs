{-# LANGUAGE ScopedTypeVariables #-}

module Day15 (day15, getNth') where

import qualified Data.IntMap as IntMap
import Data.List (elemIndex)
import Debug.Trace (trace)

day15 :: IO ()
day15 = do
  putStrLn "day15 start"
  let input = [0, 13, 16, 17, 1, 10, 6]
  let output = map (`getNth` input) [1 ..]
  print . show $ take 10 output
  print . show $ getNth 2020 input
  print . show $ getNth' 2020 input
  print . show $ getNth' 30000000 input
  putStrLn "day15 end"

getNth :: Int -> [Int] -> Int
getNth n input =
  if length input >= n
    then input !! (n - 1)
    else getNth n (input ++ [getNext input])

getNext :: [Int] -> Int
getNext input = do
  let rev = reverse input
  let n = head rev
  let ix = elemIndex n (tail rev)
  case ix of
    Nothing -> 0
    Just x -> x + 1

getNth' :: Int -> [Int] -> Int
getNth' n input = stepTo n last_val pos pos_map
 where
  last_val = last input
  pos = length input
  pos_map = getPosMap input

-- Don't put the final value into the IntMap, we need to know the last-but-one
-- position of that.
getPosMap :: [Int] -> IntMap.IntMap Int
getPosMap ps = IntMap.fromList $ zip (init ps) [1 ..]

stepTo :: Int -> Int -> Int -> IntMap.IntMap Int -> Int
stepTo n last_val pos pos_map =
  if n == pos
    then last_val
    else stepTo n next_val (pos + 1) new_pos_map
 where
  (next_val, new_pos_map) = nextStep last_val pos pos_map

nextStep :: Int -> Int -> IntMap.IntMap Int -> (Int, IntMap.IntMap Int)
nextStep last_val pos pos_map = (new_val, new_pos_map)
 where
  new_val = case IntMap.lookup last_val pos_map of
    Nothing -> 0
    Just x -> pos - x
  new_pos_map = IntMap.insert last_val pos pos_map