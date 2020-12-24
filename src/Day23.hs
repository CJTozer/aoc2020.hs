{-# LANGUAGE ScopedTypeVariables #-}

module Day23 (day23) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (elemIndex)
import Data.List.Split
import Debug.Trace

day23 :: IO ()
day23 = do
  putStrLn "day23 start"
  part1
  part2
  putStrLn "day23 end"

-- Naive list-based implementation for day 1
part1 :: IO ()
part1 = do
  let final_cups = doNMoves 100 startingPos
  print . show $ final_cups

startingPos :: [Int]
startingPos = [2, 1, 9, 3, 4, 7, 8, 6, 5]

startingPosEx :: [Int]
startingPosEx = [3, 8, 9, 1, 2, 5, 4, 6, 7]

doNMoves :: Int -> [Int] -> [Int]
doNMoves 0 cups = cups
doNMoves n cups = doNMoves (n - 1) $ doMove cups

doMove :: [Int] -> [Int]
doMove cups = do
  let current = head cups
  -- Take 3 cups from just after current cup
  let taken = take 3 . drop 1 $ cups
  -- Remaining cups stay in the circle
  let rem_cups = current : drop 4 cups
  -- Choose destination cup: highest below current that's not in taken
  let target = insertDest current taken
  -- Plonk the 3 taken in after this
  let cups' = insertAfter taken target rem_cups
  -- Find the new current cup
  let current' = bringToFront current cups' !! 1
  bringToFront current' cups'

-- Unsafe implementation
bringToFront :: (Eq a) => a -> [a] -> [a]
bringToFront y (x : xs)
  | x == y = x : xs
  | otherwise = bringToFront y (xs ++ [x])

insertDest :: Int -> [Int] -> Int
insertDest current taken =
  if prev `notElem` taken
    then prev
    else insertDest prev taken
 where
  prev = prevLabel current

prevLabel :: Int -> Int
prevLabel x = if x == 1 then 9 else x - 1

insertAfter :: (Eq a) => [a] -> a -> [a] -> [a]
insertAfter wedge target list = do
  let Just t_ix = elemIndex target list
  let (before, after) = splitAt (t_ix + 1) list
  before ++ wedge ++ after

-- Need faster IntMap-based option for part 2
part2 :: IO ()
part2 = do
  let m = startingMap
  let n = 10000000
  print . show . takeNFromMap 10 1 . doNMoves' n $ m
  print . show . product . takeNFromMap 3 1 . doNMoves' n $ m

startingPos' :: [Int]
startingPos' = startingPos ++ [10 .. 1000000]

startingMap :: IntMap Int
startingMap =
  -- And add 0 to the start to indicate the current cup is the first cup
  -- Add first cup to the end again to complete the "circle"
  addToMap ([0] ++ startingPos' ++ take 1 startingPos') IntMap.empty

addToMap :: [Int] -> IntMap Int -> IntMap Int
addToMap [] m = m
addToMap [x] m = m
addToMap (x : y : zs) m = addToMap (y : zs) $ IntMap.insert x y m

doNMoves' :: Int -> IntMap Int -> IntMap Int
doNMoves' 0 m = m
doNMoves' n m = doNMoves' (n - 1) $ doMove' m

doMove' :: IntMap Int -> IntMap Int
doMove' m = do
  -- Get current cup
  let current = currentCup m
  -- "Take" next 3 cups
  let taken = collectThreeAfter m current
  -- Link current cup to first non-taken cup
  let m' = removeTaken m
  -- Choose destination cup: highest below current that's not in taken
  let target = insertDest' current taken
  -- Plonk the 3 taken in after this
  let m'' = replaceTakenAfter taken target m'
  -- Update the new current cup
  setinsertDest m'' current

currentCup :: IntMap Int -> Int
currentCup m = m IntMap.! 0

nextCup :: IntMap Int -> Int -> Int
nextCup m x = m IntMap.! x

removeTaken :: IntMap Int -> IntMap Int
removeTaken m = IntMap.insert (currentCup m) fourth_cup m
 where
  fourth_cup = nextCup m . nextCup m . nextCup m . nextCup m $ currentCup m

collectThreeAfter :: IntMap Int -> Int -> [Int]
collectThreeAfter m x =
  [ nextCup m x
  , nextCup m . nextCup m $ x
  , nextCup m . nextCup m . nextCup m $ x
  ]

replaceTakenAfter :: [Int] -> Int -> IntMap Int -> IntMap Int
replaceTakenAfter taken target m =
  -- Target now points to first taken
  IntMap.insert target (head taken)
    .
    -- Last taken now points to one after target
    IntMap.insert (last taken) (nextCup m target)
    $ m

insertDest' :: Int -> [Int] -> Int
insertDest' current taken =
  if prev `notElem` taken
    then prev
    else insertDest' prev taken
 where
  prev = prevLabel' current

prevLabel' :: Int -> Int
prevLabel' x = if x == 1 then 1000000 else x - 1

setinsertDest :: IntMap Int -> Int -> IntMap Int
setinsertDest m current = IntMap.insert 0 (nextCup m current) m

takeNFromMap :: Int -> Int -> IntMap Int -> [Int]
takeNFromMap 1 from _ = [from]
takeNFromMap n from m = do
  let next = m IntMap.! from
  from : takeNFromMap (n - 1) next m