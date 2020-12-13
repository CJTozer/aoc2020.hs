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

import Data.List ( isInfixOf )
import Data.List.Split ( splitOn )
import Text.Regex.TDFA ( (=~) )
import qualified Data.Set as Set
import Data.Set.Extra ( flatten )

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
  let (_, _, _, submatches) = (s =~ "(.*) bags? contain (.*)") :: (String, String, String, [String])
  Rule (head submatches) $ parseContents (submatches !! 1)

parseContents :: String -> [(String, Int)]
parseContents s =
  if "no other bags" `isInfixOf` s
  then []
  else do
    let parts = splitOn "," s
    map parseContent parts

parseContent :: String -> (String, Int)
parseContent s = do
  let (_, _, _, submatches) = (s =~ " ?([0-9]+) (.*) bags?\\.?") :: (String, String, String, [String])
  (submatches !! 1, read $ head submatches)

mayContain :: Rule -> String -> Bool
mayContain r s =
  any (\ (x, _) -> x == s) (inner r)

allPossibleDirectParents :: String -> [Rule] -> Set.Set String
allPossibleDirectParents s rs = Set.fromList $ map outer $ filter (`mayContain` s) rs

allPossibleParents :: String -> [Rule] -> Set.Set String
allPossibleParents s rs = do
  let ps = allPossibleDirectParents s rs
  Set.union ps $ flatten $ Set.map (`allPossibleParents` rs) ps

bagsRequired :: String -> [Rule] -> Int
bagsRequired s rs = do
  let r = head (filter (\ r -> outer r == s) rs)
  sum (map (\ (child, num) -> num * bagsRequired child rs) (inner r)) + 1
