{-# LANGUAGE ScopedTypeVariables #-}

module Day23 (day23) where

import qualified Data.HashTable.IO as H
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (elemIndex)
import Data.List.Split
import Debug.Trace

type HashTable k v = H.BasicHashTable k v

day23 :: IO ()
day23 = do
  putStrLn "day23 start"
  -- part2
  part2'
  putStrLn "day23 end"

part2N = 10000000

part2Limit = 1000000

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
  let n = part2N
  print . show . takeNFromMap 30 1 . doNMoves' n $ m
  print . show . product . takeNFromMap 3 1 . doNMoves' n $ m

startingPos' :: [Int]
startingPos' = startingPos ++ [10 .. part2Limit]

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
  setInsertDest m'' current

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
  [ nextCup m x,
    nextCup m . nextCup m $ x,
    nextCup m . nextCup m . nextCup m $ x
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
prevLabel' x = if x == 1 then part2Limit else x - 1

setInsertDest :: IntMap Int -> Int -> IntMap Int
setInsertDest m current = IntMap.insert 0 (nextCup m current) m

takeNFromMap :: Int -> Int -> IntMap Int -> [Int]
takeNFromMap 1 from _ = [from]
takeNFromMap n from m = do
  let next = m IntMap.! from
  from : takeNFromMap (n - 1) next m

-- Use mutable map for faster part 2
part2' :: IO ()
part2' = do
  m <- startingMap'
  let n = part2N
  final_map <- doNMoves'' n m
  final_start <- takeNFromMap' 30 1 final_map
  print . show $ final_start
  final_product <- product <$> takeNFromMap' 3 1 final_map
  print . show $ final_product

startingMap' :: IO (HashTable Int Int)
startingMap' =
  -- And add 0 to the start to indicate the current cup is the first cup
  -- Add first cup to the end again to complete the "circle"
  H.fromList (zip keys values)
  where
    keys = 0 : startingPos'
    values = startingPos' ++ take 1 startingPos'

takeNFromMap' :: Int -> Int -> HashTable Int Int -> IO [Int]
takeNFromMap' 1 from _ = return [from]
takeNFromMap' n from m = do
  Just next <- H.lookup m from
  rem <- takeNFromMap' (n - 1) next m
  return (from : rem)

doNMoves'' :: Int -> HashTable Int Int -> IO (HashTable Int Int)
doNMoves'' 0 m = return m
doNMoves'' n m = doMove'' m >>= doNMoves'' (n - 1)

doMove'' :: HashTable Int Int -> IO (HashTable Int Int)
doMove'' m = do
  -- Get current cup
  current <- currentCup' m
  -- "Take" next 3 cups
  taken <- collectThreeAfter' m current
  -- Link current cup to first non-taken cup
  removeTaken' m
  -- Choose destination cup: highest below current that's not in taken
  let target = insertDest' current taken
  -- Plonk the 3 taken in after this
  replaceTakenAfter' taken target m
  -- Update the new current cup
  setInsertDest' m current
  -- Give back the final HashTable
  return m

currentCup' :: HashTable Int Int -> IO Int
currentCup' m = do
  (Just current) <- H.lookup m 0
  return current

collectThreeAfter' :: HashTable Int Int -> Int -> IO [Int]
collectThreeAfter' m x =
  sequence
    [ nextCup' m x,
      nextCup' m x >>= nextCup' m,
      nextCup' m x >>= nextCup' m >>= nextCup' m
    ]

nextCup' :: HashTable Int Int -> Int -> IO Int
nextCup' m x = do
  (Just next) <- H.lookup m x
  return next

removeTaken' :: HashTable Int Int -> IO ()
removeTaken' m = do
  current <- currentCup' m
  fourth_cup <- nextCup' m current >>= nextCup' m >>= nextCup' m >>= nextCup' m
  H.insert m current fourth_cup

replaceTakenAfter' :: [Int] -> Int -> HashTable Int Int -> IO ()
replaceTakenAfter' taken target m = do
  after_target <- nextCup' m target
  -- Target now points to first taken
  H.insert m target (head taken)
  -- Last taken now points to one after target
  H.insert m (last taken) after_target

setInsertDest' :: HashTable Int Int -> Int -> IO ()
setInsertDest' m current = do
  new_current <- nextCup' m current
  H.insert m 0 new_current