{-# LANGUAGE ScopedTypeVariables #-}

module Day5 where

import Data.List.Split

type IPtr = Int -- Instruction Pointer
type PState = [Int] -- Program state

day5 :: IO ()
day5 = do
  putStrLn "day5 start"
  contents <- readFile "data/day5"
  let final_state = runIntCode contents
  print $ show final_state
  putStrLn "day5 end"

runIntCode :: String -> PState
runIntCode s = do
  let ints = map read $ splitOn "," s
  runIntCodeFrom 0 ints

runIntCodeFrom :: IPtr -> PState -> PState
runIntCodeFrom pos ints = do
  let opcode = ints !! pos
  case opcode of
    1 -> runIntCodeFrom (pos + 4) $ opAdd pos ints
    2 -> runIntCodeFrom (pos + 4) $ opMultiply pos ints
    99 -> ints

-- Addition operation
opAdd :: IPtr -> PState -> PState
opAdd = opMath (+)

-- Multiplication operation
opMultiply :: IPtr -> PState -> PState
opMultiply = opMath (*)

-- Generic maths
opMath :: (Int -> Int -> Int) -> IPtr -> PState -> PState
opMath op pos state = do
  let ptr_a = state !! (pos + 1)
  let ptr_b = state !! (pos + 2)
  let ptr_out = state !! (pos + 3)
  let a = state !! ptr_a
  let b = state !! ptr_b
  updateValue ptr_out (op a b) state


-- Get a new state with a single uipdated value
updateValue :: IPtr -> Int -> PState -> PState
updateValue pos value state = do
  let (as, bs) = splitAt pos state
  as ++ [value] ++ tail bs
