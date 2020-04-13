{-# LANGUAGE ScopedTypeVariables #-}

module Day6 where

import Data.List.Split
import Debug.Trace

day6 :: IO ()
day6 = do
  putStrLn "day6 start"
  contents <- readFile "data/day6"
  let map = constructMap contents
  print (show map)
  putStrLn "day6 end"

type OrbitMap = OrbitElement
data OrbitElement = OrbitElement { id :: String
                                 -- , parent :: Maybe OrbitElement -- TODO Not implemented, not sure if we'll need it
                                 , children :: [OrbitElement]
                                 } deriving (Show, Eq)
type OrbitInstruction = (String, String) -- (parent, child)

constructMap :: String -> OrbitMap
constructMap s = do
  -- Split string into lines
  let ls = lines s
  -- let pairs = [(splitOn ")" l) | l <- ls]
  let pairs = map (splitOn ")") ls
  let ins = [(p!!0, p!!1) | p <- pairs]
  OrbitElement "COM" (getOrbitChildren "COM" ins)

getOrbitChildren :: String -> [OrbitInstruction] -> [OrbitElement]
getOrbitChildren id ins = do
  let direct = filter (isInstructionFor id) ins
  [ OrbitElement child_id (getOrbitChildren child_id ins)
    | (_, child_id) <- direct ]

isInstructionFor :: String -> OrbitInstruction -> Bool
isInstructionFor id ins = do
  let (parent_id, _) = ins
  parent_id == id
