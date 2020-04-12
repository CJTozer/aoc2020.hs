{-# LANGUAGE ScopedTypeVariables #-}

module Day3 where

import Data.List
import Data.List.Split
import Data.Maybe

day3 :: IO ()
day3 = do
  putStrLn "day3 start"
  contents <- readFile "data/day3"
  let ls = lines contents
  let w1:w2:_ = ls
  let closest = closestIntersection (parseWire w1) (parseWire w2)
  print (show closest)
  putStrLn "day2 end"

closestIntersection :: WireTracks -> WireTracks -> Int
closestIntersection (h1, v1) (h2, v2) = do
  let intersections = (allIntersections h1 v2) ++ (allIntersections h2 v1)
  let manhattan_ds = sort [(abs x) + (abs y) | (x, y) <- intersections]
  head (tail manhattan_ds)

data Hor = Hor { x1::Int, x2::Int, yy::Int } deriving (Show, Eq)
data Ver = Ver { xx::Int, y1::Int, y2::Int } deriving (Show, Eq)
type Point = (Int, Int)
type WireTracks = ([Hor], [Ver])
type WireState = ([String], Point, WireTracks)

-- Inclusive between returns (v1 <= x <= v2) - or v1/2 the other way around
between :: Int -> Int -> Int -> Bool
between x v1 v2 =
  x >= min v1 v2 && x <= max v1 v2

allIntersections :: [Hor] -> [Ver] -> [Point]
allIntersections hs vs = catMaybes [intersection h v | h <- hs, v <- vs]

-- Take a Hor and a Ver and return the intersection, if any
intersection :: Hor -> Ver -> Maybe Point
intersection h v = do
  let x_ok = between (xx v) (x1 h) (x2 h)
  let y_ok = between (yy h) (y1 v) (y2 v)
  case x_ok && y_ok of
    True -> Just ((xx v), (yy h))
    False -> Nothing

-- Parse a wire's tracks from the string describing it
parseWire :: String -> WireTracks
parseWire s = do
  let origin::Point = (0, 0)
  let instructions = splitOn "," s
  let (_, _, tracks) = collectInstructions (instructions, origin, ([], []))
  tracks

-- Read off the next instruction, collecting the Hor/Ver into the lists
collectInstructions :: WireState -> WireState
collectInstructions ([], p, tracks) = ([], p, tracks)
collectInstructions old_state = do
  let (op:rem_ops, (x, y), (h_tracks, v_tracks)) = old_state
  case head op of
    'U' -> do
      let new_y::Int = y + read (tail op)
      let new_v_tracks = (Ver x y new_y) : v_tracks
      collectInstructions (rem_ops, (x, new_y), (h_tracks, new_v_tracks))
    'D' -> do
      let new_y::Int = y - read (tail op)
      let new_v_tracks = (Ver x y new_y) : v_tracks
      collectInstructions (rem_ops, (x, new_y), (h_tracks, new_v_tracks))
    'R' -> do
      let new_x::Int = x + read (tail op)
      let new_h_tracks = (Hor x new_x y) : h_tracks
      collectInstructions (rem_ops, (new_x, y), (new_h_tracks, v_tracks))
    'L' -> do
      let new_x::Int = x - read (tail op)
      let new_h_tracks = (Hor x new_x y) : h_tracks
      collectInstructions (rem_ops, (new_x, y), (new_h_tracks, v_tracks))
