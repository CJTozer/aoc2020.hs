{-# LANGUAGE ScopedTypeVariables #-}

module Day24 (day24) where

import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Pos = (Int, Int)

-- data Direction = ID | E | SE | SW | W | NW | NE

day24 :: IO ()
day24 = do
  putStrLn "day24 start"
  contents <- readFile "data/day24"
  let tiles = map parse . lines $ contents
  -- part 1 answer
  print . show . nFlipped . map evaluate $ tiles

  let starting_point = accFlips (map evaluate tiles) Set.empty
  print . show . Set.size . allTilesToConsider $ starting_point
  let final_pattern = doNSteps 100 starting_point
  print . show . Set.size $ final_pattern
  putStrLn "day24 end"

parse :: String -> [Pos]
parse [] = []
parse s = case s of
  'n' : 'e' : rem -> (0, 1) : parse rem
  'e' : rem -> (1, 0) : parse rem
  's' : 'e' : rem -> (1, -1) : parse rem
  's' : 'w' : rem -> (0, -1) : parse rem
  'w' : rem -> (-1, 0) : parse rem
  'n' : 'w' : rem -> (-1, 1) : parse rem
  _ -> error ("Could not parse: " ++ s)

evaluate :: [Pos] -> Pos
evaluate [p] = p
evaluate ((x, y) : ps) = (x + x', y + y')
 where
  (x', y') = evaluate ps

nFlipped :: [Pos] -> Int
nFlipped flips = Set.size . accFlips flips $ Set.empty

accFlips :: [Pos] -> Set Pos -> Set Pos
accFlips [] flipset = flipset
accFlips (p : ps) flipset =
  if Set.member p flipset'
    then Set.delete p flipset'
    else Set.insert p flipset'
 where
  flipset' = accFlips ps flipset

-- For part 2
allTilesToConsider :: Set Pos -> Set Pos
allTilesToConsider black_tiles = Set.unions $ Set.map allNeigbours black_tiles

allNeigbours :: Pos -> Set Pos
allNeigbours (x, y) = Set.fromList [(x, y + 1), (x + 1, y), (x + 1, y -1), (x, y -1), (x -1, y), (x -1, y + 1)]

doNSteps :: Int -> Set Pos -> Set Pos
doNSteps 0 p = p
doNSteps n p = doNSteps (n - 1) p'
 where
  p' = doStep p

doStep :: Set Pos -> Set Pos
doStep black_tiles = do
  let tiles_to_check = allTilesToConsider black_tiles
  Set.filter (`shouldBeBlack` black_tiles) tiles_to_check

shouldBeBlack :: Pos -> Set Pos -> Bool
shouldBeBlack p black_tiles =
  if Set.member p black_tiles
    then -- p is black, remains black with 1 or 2 black neighbours
      n == 1 || n == 2
    else -- p is white, flips with exactly 2 black neighbours
      n == 2
 where
  n = Set.size . Set.intersection black_tiles $ allNeigbours p
