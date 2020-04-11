{-# LANGUAGE ScopedTypeVariables #-}

module Day2 where

import Data.List.Split

day2 :: IO ()
day2 = do
  putStrLn "day2 start"
  contents <- readFile "data/day2a"
  let result = intCode contents
  print $ show result
  putStrLn "day2 end"

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
