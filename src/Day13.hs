{-# LANGUAGE ScopedTypeVariables #-}

module Day13 (
  day13
, earliestBusAfter
, validPart2
, part2Inner
, parseTimetableForPart2
) where

import Data.List
import Data.List.Split
import Data.Bifunctor (first)
import Data.Function (on)
import Debug.Trace

day13 :: IO ()
day13 = do
  putStrLn "day13 start"
  contents <- readFile "data/day13"
  let (earliest_time, timetable) = (read . head . lines $ contents, splitOn "," . last . lines $ contents) :: (Int, [String])
  print $ map (earliestBusAfter earliest_time) (parseTimetable timetable)
  print $ earliestOption earliest_time (parseTimetable timetable)
  let constraints = parseTimetableForPart2 timetable
  print constraints
  print $ part2 constraints
  putStrLn "day13 end"

earliestBusAfter :: Int -> Int -> Int
earliestBusAfter time interval =
  -- Earliest bus is the first (N * interval) where (N * interval) >= time
  case time `mod` interval of
    0 -> time
    x -> time + interval - x

parseTimetable :: [String] -> [Int]
parseTimetable = map read . filter (/= "x")

earliestOption :: Int -> [Int] -> (Int, Int)
earliestOption time timetable = do
  let earliest_options = map (earliestBusAfter time) timetable
  let ordered = sortBy (compare `on` snd) (zip timetable earliest_options)
  trace (show ordered) head ordered

validPart2 :: Int -> [(Int, Int)] -> Bool
validPart2 _ [] = True
validPart2 time ((bus, offset):os) =
  (time + offset) `mod` bus == 0 &&
  validPart2 time os

parseTimetableForPart2 :: [String] -> [(Int, Int)]
parseTimetableForPart2 entries =
  map (first read) $ filter (\x -> fst x /= "x") $ zip entries [0..]

part2 :: [(Int, Int)] -> Int
part2 = part2Inner 0 1

part2Inner :: Int -> Int -> [(Int, Int)] -> Int
part2Inner start _ [] = start
part2Inner start inc (c:cs) = do
  -- All inputs are prime, so for each constraint satisfied, we know that the pattern
  -- repeats at (old interval * new bus ID) periods.
  -- Solve for next constraint only
  let start' = head $ filter (\x -> validPart2 x [c]) (map (\y -> start + inc * y) [0..])
  let inc' = inc * fst c
  part2Inner start' inc' cs
