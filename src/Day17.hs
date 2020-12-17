{-# LANGUAGE ScopedTypeVariables #-}

module Day17 (day17) where

import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Pos = (Int, Int, Int)
type State = Set Pos

day17 :: IO ()
day17 = do
  putStrLn "day17 start"
  contents <- readFile "data/day17"
  let init_state = parse . lines $ contents
  print . show $ init_state
  putStrLn "day17 end"

parse :: [String] -> State
parse ls =
  -- Always start with a 2-dimensional slice; call that z=0
  stateFromActiveCells active_cells
 where
  active_cells = getActiveCells 0 ls

-- Get all active cells for a given x-y plane, given the z-coordinate
getActiveCells :: Int -> [String] -> [Pos]
getActiveCells z ls =
  [ (x, y, z)
  | y <- [0 .. length (head ls) -1]
  , x <- [0 .. length ls - 1]
  , (ls !! x) !! y == '#'
  ]

stateFromActiveCells :: [Pos] -> State
stateFromActiveCells = Set.fromList
