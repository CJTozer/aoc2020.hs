{-# LANGUAGE ScopedTypeVariables #-}

module Day6 where

import Data.List.Split
import Data.Maybe
import Debug.Trace

day6 :: IO ()
day6 = do
  putStrLn "day6 start"
  contents <- readFile "data/day6"
  let root = constructMap contents
  let orbits = countOrbits root
  print (show orbits)
  let m = calculateManevuers root
  print (show m)
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
calculateManevuers root = do
  -- Find YOU and SAN
  let you_orbiting = findParentForId "YOU" root
  let san_orbiting = findParentForId "SAN" root
  case (you_orbiting, san_orbiting) of
    (Just x, Just y) -> maneuversBetween x y root
    _ -> 1234567890 -- Bug can't get initial orbit of YOU or SAN

maneuversBetween :: OrbitElement -> OrbitElement -> OrbitElement -> Int
maneuversBetween x y root
  -- Same element - we have a match
  | x == y = 0
  -- Otherwise move up the tree - starting with the deeper element
  | otherwise = do
      let maybe_x_parent = findParentForElem x root
      let maybe_y_parent = findParentForElem y root
      case ((elem_depth x) == (elem_depth y)) of
        -- Same elem_depth
        True -> case (maybe_x_parent, maybe_y_parent) of
          (Just x_parent, Just y_parent) -> 2 + maneuversBetween x_parent y_parent root
          _ -> trace ("Need both parents but don't have both") 789456123
        _ -> if (elem_depth x) > (elem_depth y)
          -- x deeper
          then case maybe_x_parent of
            Just x_parent -> 1 + maneuversBetween x_parent y root
            _ -> trace ("Need X parent don't have it") 456789123
          -- y deeper
          else case maybe_y_parent of
            Just y_parent -> 1 + maneuversBetween x y_parent root
            _ -> trace ("Need X parent don't have it") 789456123

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

findParentForElem :: OrbitElement -> OrbitElement -> Maybe OrbitElement
findParentForElem e root = findParentForId (elem_id e) root
