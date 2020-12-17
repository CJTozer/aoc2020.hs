{-# LANGUAGE ScopedTypeVariables #-}

module Day17 (day17) where

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
  print . show $ nextStep init_state
  let six_steps = nSteps 6 init_state
  print . show $ six_steps
  print . show $ Set.size six_steps
  putStrLn "day17 end"

parse :: [String] -> State
parse ls =
  -- Always start with a 2-dimensional slice; call that z=0
  stateFromActiveCells active_cells
 where
  active_cells = parseActiveCells 0 ls

-- Parse all active cells for the given starting x-y plane, given the z-coordinate
parseActiveCells :: Int -> [String] -> [Pos]
parseActiveCells z ls =
  [ (x, y, z)
  | y <- [0 .. length (head ls) -1]
  , x <- [0 .. length ls - 1]
  , (ls !! x) !! y == '#'
  ]

stateFromActiveCells :: [Pos] -> State
stateFromActiveCells = Set.fromList

nSteps :: Int -> State -> State
nSteps 0 s = s
nSteps n s = nSteps (n -1) (nextStep s)

nextStep :: State -> State
nextStep s =
  Set.filter (`isActiveNextStep` s) all_coords
 where
  -- TODO work out the right set of coordinates to check
  all_coords =
    Set.fromList
      [ (x, y, z)
      | x <- [-20 .. 20]
      , y <- [-20 .. 20]
      , z <- [-20 .. 20]
      ]

isActiveNextStep :: Pos -> State -> Bool
isActiveNextStep p s =
  n == 2 || (Set.member p s && n == 3)
 where
  n = numActiveNeighbours p s

numActiveNeighbours :: Pos -> State -> Int
numActiveNeighbours p s =
  Set.size $ Set.intersection s $ allNeigboursOf p

allNeigboursOf :: Pos -> State
-- TODO implement
allNeigboursOf (x, y, z) =
  Set.fromList
    [ (x', y', z')
    | x' <- [x -1, x, x + 1]
    , y' <- [y -1, y, y + 1]
    , z' <- [z -1, z, z + 1]
    , (x', y', z') /= (x, y, z)
    ]
