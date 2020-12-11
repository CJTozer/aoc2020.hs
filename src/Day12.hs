{-# LANGUAGE ScopedTypeVariables #-}

module Day12 (
  day12
, runInstructions
) where

import Data.List.Split
import Debug.Trace

type Pos = (Int, Int)
type Dir = (Int, Int)

day12 :: IO ()
day12 = do
  putStrLn "day12 start"
  contents <- readFile "data/day12"
  let final = runInstructions (0, 0) (1, 0) (lines contents)
  putStrLn . show $ final
  putStrLn "day12 end"

newFacing :: Dir -> String -> Dir
newFacing d@(x, y) s =
  case trace s s of
    "R90" -> (y, -x)
    "R180" -> (-x, -y)
    "R270" -> (-y, x)
    "L90" -> newFacing d "R270"
    "L180" -> newFacing d "R180"
    "L270" -> newFacing d "R90"

newState :: Pos -> Dir -> String -> (Pos, Dir)
newState p@(x, y) d@(xdot, ydot) s@(command:val) =
  case command of
    'E' -> ((x + intval, y), d)
    'W' -> ((x - intval, y), d)
    'N' -> ((x, y + intval), d)
    'S' -> ((x, y - intval), d)
    'F' -> ((x + xdot * intval, y + ydot * intval), d)
    _ -> ((x, y), newFacing d s)
  where intval = read val :: Int

runInstructions :: Pos -> Dir -> [String] -> (Pos, Dir)
runInstructions p d [] = (p, d)
runInstructions p d (c:cs) =
  runInstructions p' d' cs
  where (p', d') = newState p d c
