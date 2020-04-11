{-# LANGUAGE ScopedTypeVariables #-}

module Day2 where

import Data.List.Split

day2 :: IO ()
day2 = do
  putStrLn "day2 start"
  contents <- readFile "data/day2"
  let result = determineInputs contents
  print $ show result
  putStrLn "day2 end"

determineInputs :: String -> (Int, Int)
determineInputs s = do
  let ints = map read $ splitOn "," s
  let nouns = [0..99]
  let verbs = [0..99]
  checkAllInputs ints nouns verbs

checkAllInputs :: [Int] -> [Int] -> [Int] -> (Int, Int)
checkAllInputs _ _ [] = (-1, -1) -- Run out of verbs, no match :(
checkAllInputs ints [] verbs = checkAllInputs ints [0..99] (tail verbs) -- Out of nouns, move to next verb
checkAllInputs ints (n:ns) (v:vs) =
  case checkInputs ints n v of
    True -> (n, v)
    False -> checkAllInputs ints ns (v:vs)

checkInputs :: [Int] -> Int -> Int -> Bool
checkInputs ints noun verb = do
  let input = head ints : [noun, verb] ++ drop 3 ints
  let result = head $ processCmds 0 input
  case result of
    19690720 -> True
    _ -> False

intCode :: String -> [Int]
intCode s = do
  let ints = map read $ splitOn "," s
  processCmds 0 ints

processCmds :: Int -> [Int] -> [Int]
processCmds pos ints = do
  let cmd = take 4 $ drop pos ints
  case head cmd of
    99 -> ints -- Program termination
    _ -> processCmds (pos + 4) $ doCmd (cmd !! 0, cmd !! 1, cmd !! 2, cmd !! 3) ints

-- operate on them - new function taking 4, returning (pos, value)
-- create a new array with the replaced value
doCmd :: (Int, Int, Int, Int) -> [Int] -> [Int]
doCmd (op, x_pos, y_pos, o_pos) ints = do
  -- Addition
  let (as, bs) = splitAt o_pos ints
  let new_val = case op of
        1 -> ints !! x_pos + ints !! y_pos
        2 -> ints !! x_pos * ints !! y_pos
        _ -> -1 -- Illegal opcode!
  as ++ [new_val] ++ tail bs
