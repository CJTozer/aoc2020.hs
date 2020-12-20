{-# LANGUAGE ScopedTypeVariables #-}

module Day20 (
  day20,
  fillGrid,
  naturalTileEdges,
  flipTile,
  rotateTile,
  addAllSerpents,
  renderFinalGrid,
  Tile (..),
  Grid,
) where

import Data.List (delete, transpose)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

type Pos = (Int, Int)
data Tile = Tile {identifier :: Int, raw_data :: [String]} deriving (Show, Eq)
type Grid = Map Pos Tile
type Render = Map Pos Char

day20 :: IO ()
day20 = do
  putStrLn "day20 start"
  contents <- readFile "data/day20"
  let tiles = parse contents
  print . show . head $ tiles
  print . show . possibleTileEdges . head $ tiles
  let all_edges = concatMap possibleTileEdges tiles
  let edge_counts = getEdgeCounts all_edges
  print . show $ map (`numNonMatchingEdges` edge_counts) tiles
  -- Part 1: corners should be tiles with 2 non-matching edges (if the data allows!)
  print . show . filter (\x -> snd x == 2) $ map (`numNonMatchingEdges` edge_counts) tiles
  print . show . product . map fst . filter (\x -> snd x == 2) $ map (`numNonMatchingEdges` edge_counts) tiles
  -- Part 2: Anything should be able to fill (0,0) at the start
  print . show . map identifier . filter (\t -> canFillSlot t (0, 0) Map.empty) $ tiles
  -- Bung 3373 (an initial corner) at (0,0) to start us off
  let init_corner_id = 3373
  let init_corner = rotateTile . rotateTile . head . filter (\t -> identifier t == init_corner_id) $ tiles
  let rem_tiles = filter (\t -> identifier t /= init_corner_id) tiles
  let init_grid = Map.fromList [((0, 0), init_corner)]
  -- Now should have fewer options for (1, 0), (0, 1), (-1, 0), (0, -1)
  print . show . filter (\t -> canFillSlot t (1, 0) init_grid) $ rem_tiles
  print . show . filter (\t -> canFillSlot t (0, 1) init_grid) $ rem_tiles
  print . show . filter (\t -> canFillSlot t (-1, 0) init_grid) $ rem_tiles
  print . show . filter (\t -> canFillSlot t (0, -1) init_grid) $ rem_tiles
  print . show $ nextPos 12 init_grid
  -- Our data is a 12 * 12 grid; let's fill it in
  let final_grid = fillGrid 12 init_grid rem_tiles
  let final_render = renderFinalGrid final_grid
  print . show . length . Map.toList $ final_render
  print . show . length . filter (\x -> snd x == '#') . Map.toList $ final_render
  -- Get the 'O's for the serpents
  let serpent_map = serpentMap final_render
  print . show $ serpent_map
  print . show . length $ serpent_map
  print . show . Set.size . Set.fromList $ serpent_map
  putStrLn "day20 end"

parse :: String -> [Tile]
parse s = map parseTile (splitOn [""] . lines $ s)

parseTile :: [String] -> Tile
parseTile (id_line : grid_lines) = Tile{identifier = x, raw_data = grid_lines}
 where
  x = read . init . drop 5 $ id_line

getEdgeCounts :: [String] -> Map String Int
getEdgeCounts = foldr (\e -> Map.insertWith (+) e 1) Map.empty

possibleTileEdges :: Tile -> [String]
possibleTileEdges t = naturalTileEdges t >>= (\x -> [x, reverse x])

-- Must be in order: up, right, down, left - and all read _clockwise_
naturalTileEdges :: Tile -> [String]
naturalTileEdges t = map (\f -> f $ raw_data t) [head, map last, reverse . last, reverse . map head]

numNonMatchingEdges :: Tile -> Map String Int -> (Int, Int)
numNonMatchingEdges t counts = (identifier t, length . filter (\e -> Map.findWithDefault 0 e counts < 2) $ naturalTileEdges t)

-- Part 2 actually requires us to arrange the grid
canFillSlot :: Tile -> Pos -> Grid -> Bool
canFillSlot tile pos grid = canMatchEdges adj_edges tile
 where
  adj_edges = getAdjEdges pos grid

canMatchEdges :: [Maybe String] -> Tile -> Bool
canMatchEdges constraints tile = any (permutationMatches constraints) all_permutations
 where
  all_permutations = (allRotations . naturalTileEdges) tile ++ (allRotations . naturalTileEdges . flipTile) tile

permutationMatches :: [Maybe String] -> [String] -> Bool
permutationMatches constraints sides = all sideMatches $ zip constraints sides

sideMatches :: (Maybe String, String) -> Bool
sideMatches (Nothing, _) = True
sideMatches (Just c, s) = c == s

allRotations :: [String] -> [[String]]
allRotations xs = take (length xs) (iterate (\(y : ys) -> ys ++ [y]) xs)

-- Adjacent edges are ordered clockwise: up, right, down, left - but need to reverse themas they're currently read "from" the centre of their cell not the one we're testing
getAdjEdges :: Pos -> Grid -> [Maybe String]
getAdjEdges (x, y) grid =
  [ getTileAtPos (x, y + 1) grid >>= \x -> Just $ reverse (getLowerEdge x)
  , getTileAtPos (x + 1, y) grid >>= \x -> Just $ reverse (getLeftEdge x)
  , getTileAtPos (x, y - 1) grid >>= \x -> Just $ reverse (getUpperEdge x)
  , getTileAtPos (x - 1, y) grid >>= \x -> Just $ reverse (getRightEdge x)
  ]

getLeftEdge :: Tile -> String
getLeftEdge = reverse . map head . raw_data

getRightEdge :: Tile -> String
getRightEdge = map last . raw_data

getUpperEdge :: Tile -> String
getUpperEdge = head . raw_data

getLowerEdge :: Tile -> String
getLowerEdge = reverse . last . raw_data

getTileAtPos :: Pos -> Grid -> Maybe Tile
getTileAtPos = Map.lookup

-- Take the current grid & available tiles, and fill the specified space
fillPosWith :: Pos -> Grid -> [Tile] -> (Grid, [Tile])
fillPosWith p grid ts =
  case fits of
    [t] -> (Map.insert p t' grid, delete t ts)
     where
      t' = rotateToFit t p grid
    x -> error ("Don't know how to handle; have not got one answer: " ++ show x)
 where
  fits = filter (\t -> canFillSlot t p grid) ts

rotateToFit :: Tile -> Pos -> Grid -> Tile
rotateToFit tile pos grid =
  case matches of
    [m] -> m
    x -> error ("rotateToFit: Don't know to handle multiple matches: " ++ show x)
 where
  matches = filter (permutationMatches constraints . naturalTileEdges) possible_tiles
  constraints = getAdjEdges pos grid
  possible_tiles =
    map
      (\f -> f tile)
      [ id
      , rotateTile
      , rotateTile . rotateTile
      , rotateTile . rotateTile . rotateTile
      , flipTile
      , flipTile . rotateTile
      , flipTile . rotateTile . rotateTile
      , flipTile . rotateTile . rotateTile . rotateTile
      ]

rotateTile :: Tile -> Tile
rotateTile t = t{raw_data = transpose . reverse $ raw_data t}

flipTile :: Tile -> Tile
flipTile t = t{raw_data = reverse $ raw_data t}

-- Fill a square grid
fillGrid :: Int -> Grid -> [Tile] -> Grid
fillGrid _ grid [] = grid
fillGrid side grid tiles = fillGrid side grid' tiles'
 where
  (grid', tiles') = fillPosWith p grid tiles
  p = nextPos side grid

-- Get next position to fill (square grid)
nextPos :: Int -> Grid -> Pos
nextPos side grid =
  head . filter (`Map.notMember` grid) $
    [ (x, y)
    | x <- [0 .. side - 1]
    , y <- [0 .. side - 1]
    ]

-- Render final grid
renderFinalGrid :: Grid -> Render
renderFinalGrid grid = renderFinalGrid' (Map.toList grid) Map.empty

renderFinalGrid' :: [(Pos, Tile)] -> Render -> Render
renderFinalGrid' [] r = r
renderFinalGrid' ((p, t) : xs) render = renderFinalGrid' xs render'
 where
  render' = addTileToRender p t render

addTileToRender :: Pos -> Tile -> Render -> Render
addTileToRender (x, y) t = addStringsToRender x_o y_o ls
 where
  -- Tiles are 10x10 but we need to remove the borders
  x_o = x * 8
  y_o = y * 8
  -- Put lines in order of increasing x, remove borders (first and last)
  ls = (tail . reverse) $ (tail . raw_data) t

addStringsToRender :: Int -> Int -> [String] -> Render -> Render
addStringsToRender _ _ [] render = render
addStringsToRender x y (l : ls) render = addStringsToRender x (y + 1) ls render'
 where
  -- Remove first and last from row, since we need to drop the border
  render' = addRowToRender x y (init . tail $ l) render

addRowToRender :: Int -> Int -> String -> Render -> Render
addRowToRender _ _ "" render = render
addRowToRender x y (c : cs) render = addRowToRender (x + 1) y cs render'
 where
  render' = case Map.lookup (x, y) render of
    Nothing -> Map.insert (x, y) c render
    Just n ->
      if n == c
        then Map.insert (x, y) c render
        else error ("Inserting " ++ show c ++ " at position " ++ show (x, y) ++ " when it already contained " ++ show x)

-- Find the damn serpents.
-- Serpent pattern is:
--                   #
-- #    ##    ##    ###
--  #  #  #  #  #  #
-- So if we take the left-most '#' as the origin, we need these relative coordinates to be '#'
basicSerpentMatch :: [Pos]
basicSerpentMatch = [(0, 0), (1, -1), (4, -1), (5, 0), (6, 0), (7, -1), (10, -1), (11, 0), (12, 0), (13, -1), (16, -1), (17, 0), (18, 1), (18, 0), (19, 0)]

rot90 :: Pos -> Pos
rot90 (x, y) = (- y, x)
rot180 :: Pos -> Pos
rot180 = rot90 . rot90
rot270 :: Pos -> Pos
rot270 = rot90 . rot180
flipAboutY :: Pos -> Pos
flipAboutY (x, y) = (- x, y)

allSerpentMatches :: [[Pos]]
allSerpentMatches =
  map
    (\f -> map f basicSerpentMatch)
    [ id
    , rot90
    , rot180
    , rot270
    , flipAboutY
    , rot90 . flipAboutY
    , rot180 . flipAboutY
    , rot270 . flipAboutY
    ]

-- Build a list of serpent squares (with duplicates, for speed)
serpentMap :: Render -> [Pos]
serpentMap render = addAllSerpents render allSerpentMatches []

addAllSerpents :: Render -> [[Pos]] -> [Pos] -> [Pos]
addAllSerpents _ [] serpent_map = serpent_map
addAllSerpents render (serp : serps) serpent_map = addAllSerpents render serps serpent_map'
 where
  serpent_map' = addSerpents render serp serpent_map

addSerpents :: Render -> [Pos] -> [Pos] -> [Pos]
addSerpents render serpent serpent_map = addSerpentsAt valid_coords serpent serpent_map
 where
  valid_coords = filter (\p -> validSerpentAt p render serpent) [(x, y) | x <- [-20 .. 120], y <- [-20 .. 120]]

validSerpentAt :: Pos -> Render -> [Pos] -> Bool
validSerpentAt p render = all ((== Just '#') . (`Map.lookup` render) . sumPos p)

sumPos :: Pos -> Pos -> Pos
sumPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

addSerpentsAt :: [Pos] -> [Pos] -> [Pos] -> [Pos]
addSerpentsAt [] _ serpent_map = serpent_map
addSerpentsAt (o : os) serpent serpent_map = addSerpentsAt os serpent serpent_map'
 where
  serpent_map' = addSerpentAt o serpent serpent_map

addSerpentAt :: Pos -> [Pos] -> [Pos] -> [Pos]
addSerpentAt origin serpent serpent_map = serpent_map ++ new_squares
 where
  new_squares = map (sumPos origin) serpent