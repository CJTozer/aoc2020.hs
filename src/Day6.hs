{-# LANGUAGE ScopedTypeVariables #-}

module Day6 where

import Data.List.Split
import Data.Maybe
import Debug.Trace

day6 :: IO ()
day6 = do
  putStrLn "day6 start"
  contents <- readFile "data/day6"
  let map = constructMap contents
  let orbits = countOrbits map
  print (show orbits)
  putStrLn "day6 end"

data OrbitElement = OrbitElement { elem_id :: String
                                 , children :: [OrbitElement]
                                 , elem_depth :: Int
                                 } deriving (Show, Eq)
type OrbitInstruction = (String, String) -- (parent, child)

constructMap :: String -> OrbitElement
constructMap s = do
  -- Split string into lines
  let ls = lines s
  -- let pairs = [(splitOn ")" l) | l <- ls]
  let pairs = map (splitOn ")") ls
  let ins = [(p!!0, p!!1) | p <- pairs]
  OrbitElement "COM" (getOrbitChildren "COM" 1 ins) 0

getOrbitChildren :: String -> Int -> [OrbitInstruction] -> [OrbitElement]
getOrbitChildren id depth ins = do
  let direct = filter (isInstructionFor id) ins
  [ OrbitElement child_id (getOrbitChildren child_id (depth + 1) ins) depth
    | (_, child_id) <- direct ]

isInstructionFor :: String -> OrbitInstruction -> Bool
isInstructionFor id ins = do
  let (parent_id, _) = ins
  parent_id == id

countOrbits :: OrbitElement -> Int
countOrbits elem = (elem_depth elem) + sum (map countOrbits (children elem))

calculateManevuers :: OrbitElement -> Int
calculateManevuers map = do
  -- Find YOU and SAN
  -- If not same depth, move lower up parents until matching
  -- If same depth and same parent - bingpot
  -- Otherwise move both up one parent
  undefined

-- This is all inefficient and we should build a better data structure...  But this is easier!
findElementById :: String -> OrbitElement -> Maybe OrbitElement
findElementById id root = do
  -- Is it the map root?
  if id == (elem_id root)
  then Just root
  else do
    -- Is it in the root's children?
    let matching_child = listToMaybe $ filter (\e -> id == elem_id e) (children root)
    case matching_child of
      Just x -> Just x
      -- No match in children - recurse.  Hopefully this will be lazy!
      Nothing -> listToMaybe $ catMaybes $ map (findElementById id) (children root)

findParentForId :: String -> OrbitElement -> Maybe OrbitElement
findParentForId id root = do
  -- Is it in the root's children?
  let matching_child = listToMaybe $ filter (\e -> id == elem_id e) (children root)
  case matching_child of
    Just x -> Just root
    -- No match in children - recurse.  Hopefully this will be lazy!
    Nothing -> listToMaybe $ catMaybes $ map (findParentForId id) (children root)
