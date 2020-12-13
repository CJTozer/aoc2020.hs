{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (
  day5,
  seatToId,
) where

import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Set as Set

day5 :: IO ()
day5 = do
  putStrLn "day5 start"
  contents <- readFile "data/day5"
  print $ maximum $ map seatToId $ lines contents
  print $ findMissing $ map seatToId $ lines contents
  putStrLn "day5 end"

seatToId :: String -> Int
seatToId =
  toDec
    . replace 'F' '0'
    . replace 'B' '1'
    . replace 'R' '1'
    . replace 'L' '0'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

replace :: Eq a => a -> a -> [a] -> [a]
replace x x' = map (\c -> if c == x then x' else c)

findMissing :: [Int] -> [Int]
findMissing ids = do
  let s = Set.fromList ids
  Set.toAscList $ Set.filter (\x -> Set.notMember (x + 1) s || Set.notMember (x -1) s) s
