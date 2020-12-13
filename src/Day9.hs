{-# LANGUAGE ScopedTypeVariables #-}

module Day9 (day9) where

day9 :: IO ()
day9 = do
  putStrLn "day9 start"
  contents <- readFile "data/day9"
  let ints = parse contents
  print $ take 5 ints
  let inv = findFirstInvalid ints
  print inv
  let range = findRangeSummingTo inv ints
  print range
  print $ minimum range + maximum range
  putStrLn "day9 end"

parse :: String -> [Int]
parse s = map read $ lines s

pairSums :: [Int] -> [Int]
pairSums [] = []
pairSums [_] = []
pairSums (x:xs) = pairSums' x xs ++ pairSums xs

pairSums' :: Int -> [Int] -> [Int]
pairSums' n = map (n +)

canBeMadeFromPair :: Int -> [Int] -> Bool
canBeMadeFromPair n ints = n `elem` pairSums ints

nthNumValid :: Int -> [Int] -> (Int, Bool)
nthNumValid n ints = do
  -- n is zero-indexed
  -- Previous 25 numbers
  let last25 = lastN 25 . take n $ ints
  let x = ints !! n
  (x, canBeMadeFromPair x last25)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

findFirstInvalid :: [Int] -> Int
findFirstInvalid ints = do
  let res = nthNumValid 25 ints
  if snd res
  then
    findFirstInvalid $ tail ints
  else
    fst res

takeSummingToAtLeast :: Int -> [Int] -> [Int]
takeSummingToAtLeast n (x:xs) =
  if x < n
    then x : takeSummingToAtLeast (n - x) xs
    else [x]

findRangeSummingTo :: Int -> [Int] -> [Int]
findRangeSummingTo n ints = do
  let r = takeSummingToAtLeast n ints
  if sum r == n
    then r
    else findRangeSummingTo n $ tail ints
