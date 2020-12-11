{-# LANGUAGE ScopedTypeVariables #-}

module Day12b (
  day12b
, runInstructions
) where

import Data.List.Split
import Debug.Trace

type Pos = (Int, Int)
type Dir = (Int, Int)

day12b :: IO ()
day12b = do
  putStrLn "day12b start"
  contents <- readFile "data/day12"
  let final = runInstructions (0, 0) (10, 1) (lines contents)
  putStrLn . show $ final
  putStrLn "day12b end"

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
    -- NSEW move the waypoint d
    'E' -> (p, (xdot + intval, ydot))
    'W' -> (p, (xdot - intval, ydot))
    'N' -> (p, (xdot, ydot + intval))
    'S' -> (p, (xdot, ydot - intval))
    -- F moves N times along the waypoint vector d
    'F' -> ((x + xdot * intval, y + ydot * intval), d)
    -- R and L still have the same effect
    _ -> ((x, y), newFacing d s)
  where intval = read val :: Int

runInstructions :: Pos -> Dir -> [String] -> (Pos, Dir)
runInstructions p d [] = (p, d)
runInstructions p d (c:cs) =
  runInstructions p' d' cs
  where (p', d') = newState p d c
