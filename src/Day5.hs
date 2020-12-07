{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (
  day5
, seatToId
  ) where

import Data.List.Split
import Data.Char (digitToInt)
import Data.List (foldl', sort)
import qualified Data.Set as Set
import Debug.Trace

day5 :: IO ()
day5 = do
  putStrLn "day5 start"
  contents <- readFile "data/day5"
  print $ show $ head $ reverse $ sort $ map seatToId $ lines contents
  print $ show $ findMissing $ map seatToId $ lines contents
  putStrLn "day5 end"

seatToId :: String -> Int
seatToId s =
  toDec $
  replace 'F' '0' $
  replace 'B' '1' $
  replace 'R' '1' $
  replace 'L' '0' s

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

replace :: Eq a => a -> a -> [a] -> [a]
replace x x' s = map (\c -> if c==x then x'; else c) s

findMissing :: [Int] -> [Int]
findMissing ids = do
  let s = Set.fromList ids
  Set.toAscList $ Set.filter (\x -> Set.notMember (x+1) s || Set.notMember (x-1) s) s
