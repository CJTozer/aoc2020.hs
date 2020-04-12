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
  let (w1_tracks, w1_ps) = parseWire w1
  let (w2_tracks, w2_ps) = parseWire w2
  let closest = closestIntersection w1_tracks w2_tracks
  print (show closest)
  putStrLn "day2 end"

-- Get the intersection with the shortest delay
fastestIntersection :: WireData-> WireData -> Int
fastestIntersection ((h1, v1), ps1) ((h2, v2), ps2) = do
  let intersections = (allIntersections h1 v2) ++ (allIntersections h2 v1)
-- TODO
  let manhattan_ds = sort [(abs x) + (abs y) | (x, y) <- intersections]
  head (tail manhattan_ds)

distanceToIntersection :: Point -> [Point] -> Int
distanceToIntersection t ps = do
  -- p0 is the current point
  -- p1 is the next point
  -- t is the target point
  let p0:p1:_ = ps
  case onLine t p0 p1 of
    -- If t is on the line between p0 and p1, we will hit our intersection
    True -> distanceBetween p0 t
    -- If not, move to p1 and continue searching from there
    False -> (distanceBetween p0 p1) + distanceToIntersection t (tail ps)

-- Given 2 points and a target point, work out if the line between the first two contains the target
onLine :: Point -> Point -> Point -> Bool
onLine p1 p2 t = do
  let (p1_x, p1_y) = p1
  let (p2_x, p2_y) = p2
  let (t_x, t_y) = t
  or [
    and [and $ map (== t_x) [p1_x, p2_x], between t_y p1_y p2_y],
    and [and $ map (== t_y) [p1_y, p2_y], between t_x p1_x p2_x]
     ]

-- Work out the Manhattan distance between two points
distanceBetween :: Point -> Point -> Int
distanceBetween p1 p2 = 0 -- TODO

-- Get the intersection with the closest (Manhattan) distance to the origin
closestIntersection :: WireTracks -> WireTracks -> Int
closestIntersection (h1, v1) (h2, v2) = do
  let intersections = (allIntersections h1 v2) ++ (allIntersections h2 v1)
  let manhattan_ds = sort [(abs x) + (abs y) | (x, y) <- intersections]
  head (tail manhattan_ds)

data Hor = Hor { x1::Int, x2::Int, yy::Int } deriving (Show, Eq)
data Ver = Ver { xx::Int, y1::Int, y2::Int } deriving (Show, Eq)
type Point = (Int, Int)
type WireTracks = ([Hor], [Ver])
type WireData = (WireTracks, [Point])
type WireState = ([String], [Point], WireTracks)

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
parseWire :: String -> (WireTracks, [Point])
parseWire s = do
  let origin::Point = (0, 0)
  let instructions = splitOn "," s
  let (_, points, tracks) = collectInstructions (instructions, origin:[], ([], []))
  (tracks, points)

-- Read off the next instruction, collecting the Hor/Ver into the lists
collectInstructions :: WireState -> WireState
collectInstructions ([], ps, tracks) = ([], ps, tracks)
collectInstructions old_state = do
  let (op:rem_ops, ps, (h_tracks, v_tracks)) = old_state
  let (x, y) = head ps
  case head op of
    'U' -> do
      let new_y::Int = y + read (tail op)
      let new_v_tracks = (Ver x y new_y) : v_tracks
      collectInstructions (rem_ops, (x, new_y):ps, (h_tracks, new_v_tracks))
    'D' -> do
      let new_y::Int = y - read (tail op)
      let new_v_tracks = (Ver x y new_y) : v_tracks
      collectInstructions (rem_ops, (x, new_y):ps, (h_tracks, new_v_tracks))
    'R' -> do
      let new_x::Int = x + read (tail op)
      let new_h_tracks = (Hor x new_x y) : h_tracks
      collectInstructions (rem_ops, (new_x, y):ps, (new_h_tracks, v_tracks))
    'L' -> do
      let new_x::Int = x - read (tail op)
      let new_h_tracks = (Hor x new_x y) : h_tracks
      collectInstructions (rem_ops, (new_x, y):ps, (new_h_tracks, v_tracks))
