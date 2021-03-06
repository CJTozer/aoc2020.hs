{-# LANGUAGE ScopedTypeVariables #-}

module Day11 (day11) where

import Debug.Trace (trace)

type Pos = (Int, Int)

day11 :: IO ()
day11 = do
  putStrLn "day11 start"
  contents <- readFile "data/day11"
  let initial_state = lines contents
  print initial_state
  print $ newFullState initial_state
  print $ findSteadyState initial_state
  putStrLn "day11 end"

numOccupied :: [String] -> Int
numOccupied = sum . map (length . filter (== '#'))

findSteadyState :: [String] -> [String]
findSteadyState s =
  if trace (show $ numOccupied s) new_s == s
    then s
    else findSteadyState new_s
 where
  new_s = newFullState s

newFullState :: [String] -> [String]
newFullState s =
  map (`newRowState` s) [0 .. (length s - 1)]

newRowState :: Int -> [String] -> String
newRowState row_n s =
  map (`newState` s) ps
 where
  ps = [(row_n, y) | y <- [0 .. (length (head s) - 1)]]

newState :: Pos -> [String] -> Char
newState p s =
  case this_seat of
    '.' -> '.'
    '#' ->
      if occupied_neighbours >= 5
        then 'L'
        else '#'
    'L' ->
      if occupied_neighbours == 0
        then '#'
        else 'L'
 where
  this_seat = seatAtPos p s
  occupied_neighbours = countOccupiedNeighbours p s

-- Safe - anything out-of-bounds is (a special char representing) floor
seatAtPos :: Pos -> [String] -> Char
seatAtPos (x, y) s =
  if (x < 0) || (x >= length s) || (y < 0) || (y >= length (head s))
    then ','
    else (s !! x) !! y

countOccupiedNeighbours :: Pos -> [String] -> Int
countOccupiedNeighbours p s =
  sum . map isOccupied $ getNeighbours' p s

-- 0 for unoccupied, 1 for occupied
isOccupied :: Char -> Int
isOccupied c = if c == '#' then 1 else 0

getNeighbours :: Pos -> [String] -> String
getNeighbours (x, y) s =
  [ seatAtPos (x', y') s
  | x' <- [x -1, x, x + 1]
  , y' <- [y -1, y, y + 1]
  , (x' /= x) || (y' /= y)
  ]

getNeighbours' :: Pos -> [String] -> String
getNeighbours' p s =
  [ getNeighbourOnVector p (vx, vy) s
  | vx <- [-1, 0, 1]
  , vy <- [-1, 0, 1]
  , (vx /= 0) || (vy /= 0)
  ]

getNeighbourOnVector :: Pos -> Pos -> [String] -> Char
getNeighbourOnVector (px, py) v@(vx, vy) s =
  case seat of
    -- Keep going if floor, otherwise return the value found
    '.' -> getNeighbourOnVector p' v s
    x -> x
 where
  p' = (px + vx, py + vy)
  seat = seatAtPos p' s
