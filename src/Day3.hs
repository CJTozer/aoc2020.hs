{-# LANGUAGE ScopedTypeVariables #-}

module Day3 where

import Data.List.Split

day2 :: IO ()
day2 = do
  putStrLn "day3 start"
  contents <- readFile "data/day3"
  let ls = lines contents
  let w1:w2:_ = ls
  putStrLn "day2 end"

data Hor = Hor { x1::Int, x2::Int, yy::Int }
data Ver = Ver { xx::Int, y1::Int, y2::Int }
type Point = (Int, Int)
type WireTracks = ([Hor], [Ver])
type WireState = ([String], Point, WireTracks)

-- Inclusive between returns (v1 <= x <= v2) - or v1/2 the other way around
between :: Int -> Int -> Int -> Bool
between x v1 v2 =
  x >= min v1 v2 && x <= max v1 v2

-- Take a Hor and a Ver and return the intersection, if any
intersection :: Hor -> Ver -> Maybe Point
intersection h v = do
  let x_ok = between (xx v) (x1 h) (x2 h)
  let y_ok = between (yy h) (y1 v) (y2 v)
  case x_ok && y_ok of
    True -> Just ((xx v), (yy h))
    False -> Nothing

parseWire :: String -> WireTracks
parseWire s = do
  let origin::Point = (0, 0)
  let instructions = splitOn "," s
  ([], [])

-- Read off the next instruction, collecting the Hor/Ver into the lists
collectInstructions :: WireState -> WireState
collectInstructions ([], p, tracks) = ([], p, tracks)
collectInstructions old_state = do
  let (op:rem_ops, (x, y), (h_tracks, v_tracks)) = old_state
  case head op of
    'U' -> do
      let new_y::Int = y + read (tail op)
      let new_v_tracks = (Ver x y new_y) : v_tracks
      (rem_ops, (x, new_y), (h_tracks, new_v_tracks))
    'D' -> do
      let new_y::Int = y - read (tail op)
      let new_v_tracks = (Ver x y new_y) : v_tracks
      (rem_ops, (x, new_y), (h_tracks, new_v_tracks))
    'R' -> do
      let new_x::Int = x + read (tail op)
      let new_h_tracks = (Hor x new_x y) : h_tracks
      (rem_ops, (new_x, y), (new_h_tracks, v_tracks))
    'L' -> do
      let new_x::Int = x - read (tail op)
      let new_h_tracks = (Hor x new_x y) : h_tracks
      (rem_ops, (new_x, y), (new_h_tracks, v_tracks))
