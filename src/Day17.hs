{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day17 (day17) where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

type Pos = (Int, Int, Int)
data Cell = Inactive | Active deriving (Show)
type State = Map Pos Cell

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
  | x <- [0 .. length ls - 1]
  -- TODO only get the active ones!
  ]

stateFromActiveCells :: [Pos] -> State
stateFromActiveCells = foldr (`Map.insert` Active) Map.empty

getCell :: Pos -> State -> Cell
getCell = Map.findWithDefault Inactive
