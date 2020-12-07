{-# LANGUAGE ScopedTypeVariables #-}

module Day7 (
  day7
, Rule (Rule)
, parseRules
, lineToRule
, mayContain
, allPossibleParents
, bagsRequired
) where

import Data.List
import Data.List.Split
import Text.Regex.TDFA
import qualified Data.Set as Set
import Data.Set.Extra (flatten)
import Debug.Trace

data Rule = Rule { outer::String, inner::[(String, Int)] } deriving (Show, Eq)

day7 :: IO ()
day7 = do
  putStrLn "day7 start"
  contents <- readFile "data/day7"
  let rs = parseRules contents
  let ps = allPossibleParents "shiny gold" rs
  print $ show $ length ps
  print $ show $ bagsRequired "shiny gold" rs
  putStrLn "day7 end"

parseRules :: String -> [Rule]
parseRules s = map lineToRule $ lines s

lineToRule :: String -> Rule
lineToRule s = do
  let (before, match, after, submatches) = (s =~ "(.*) bags? contain (.*)") :: (String, String, String, [String])
  Rule (submatches !! 0) $ parseContents (submatches !! 1)

parseContents :: String -> [(String, Int)]
parseContents s = do
  if (isInfixOf "no other bags" s)
  then []
  else do
    let parts = splitOn "," s
    map parseContent parts

parseContent :: String -> (String, Int)
parseContent s = do
  let (before, match, after, submatches) = (s =~ " ?([0-9]+) (.*) bags?\\.?") :: (String, String, String, [String])
  (submatches !! 1, read $ submatches !! 0)

mayContain :: Rule -> String -> Bool
mayContain r s =
  (length $ filter (\(x, y) -> x == s) (inner r)) > 0

allPossibleDirectParents :: String -> [Rule] -> Set.Set(String)
allPossibleDirectParents s rs = Set.fromList $ map outer $ filter (\r -> mayContain r s) rs

allPossibleParents :: String -> [Rule] -> Set.Set(String)
allPossibleParents s rs = do
  let ps = allPossibleDirectParents s rs
  Set.union ps $ flatten $ Set.map (\p -> allPossibleParents p rs) ps

bagsRequired :: String -> [Rule] -> Int
bagsRequired s rs = do
  let r = (filter (\r -> outer r == s ) rs) !! 0
  (sum $ map (\(child, num) -> num * (bagsRequired child rs)) (inner r)) + 1
