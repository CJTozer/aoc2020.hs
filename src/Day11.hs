{-# LANGUAGE ScopedTypeVariables #-}

module Day11 (day11) where

import Data.List.Split
import Debug.Trace

type Pos = (Int, Int)

day11 :: IO ()
day11 = do
  putStrLn "day11 start"
  contents <- readFile "data/day11"
  let initial_state = lines contents
  putStrLn . show $ initial_state
  putStrLn . show $ newFullState initial_state
  putStrLn . show $ findSteadyState initial_state
  putStrLn "day11 end"

numOccupied :: [String] -> Int
numOccupied s = sum . (map length) . map (filter (=='#')) $ s

findSteadyState :: [String] -> [String]
findSteadyState s =
  if trace (show $ numOccupied s) new_s == s
  then s
  else findSteadyState new_s
  where new_s = newFullState s

newFullState :: [String] -> [String]
newFullState s =
  map (\n -> newRowState n s) [0..(length s - 1)]

newRowState :: Int -> [String] -> String
newRowState row_n s =
  map (\p -> newState p s) ps
  where ps = [(row_n, y) | y <- [0..(length (head s) - 1)]]

newState :: Pos -> [String] -> Char
newState p@(x, y) s =
  case this_seat of
    '.' -> '.'
    '#' -> if occupied_neighbours >= 4
      then 'L'
      else '#'
    'L' -> if occupied_neighbours == 0
      then '#'
      else 'L'
  where
    this_seat = seatAtPos p s
    occupied_neighbours = countOccupiedNeighbours p s

-- Safe - anything out-of-bounds is floor
seatAtPos :: Pos -> [String] -> Char
seatAtPos (x, y) s =
  if (x < 0) || (x >= length s) || (y < 0) || (y >= length (head s))
  then '.'
  else (s !! x) !! y

countOccupiedNeighbours :: Pos -> [String] -> Int
countOccupiedNeighbours p s =
  sum . (map isOccupied) $ getNeighbours p s

-- 0 for unoccupied, 1 for occupied
isOccupied :: Char -> Int
isOccupied c = if (c == '#') then 1 else 0

getNeighbours :: Pos -> [String] -> [Char]
getNeighbours (x, y) s =
  [seatAtPos (x', y') s |
   x' <- [x-1, x, x+1],
   y' <- [y-1, y, y+1],
   (x' /= x) || (y' /= y)]
