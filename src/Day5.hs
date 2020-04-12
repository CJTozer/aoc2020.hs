{-# LANGUAGE ScopedTypeVariables #-}

module Day5 where

import Data.List.Split
import Debug.Trace

type IPtr = Int -- Instruction Pointer
type PState = [Int] -- Program state
type OpCode = Int

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
  case rem opcode 100 of
    1 -> runIntCodeFrom (pos + 4) $ opAdd pos ints
    2 -> runIntCodeFrom (pos + 4) $ opMultiply pos ints
    3 -> runIntCodeFrom (pos + 2) $ opInput pos ints
    4 -> runIntCodeFrom (pos + 2) $ opOutput pos ints
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
  let ptrs = getPointersFromOpPtr 3 pos state
  let ptr_a = ptrs !! 0
  let ptr_b = ptrs !! 1
  let ptr_out = ptrs !! 2

  let a = state !! ptr_a
  let b = state !! ptr_b
  updateValue ptr_out (op a b) state

-- Input operation
-- Cheating for now by knowing the input should be 1
opInput :: IPtr -> PState -> PState
opInput pos state = do
  let ptrs = getPointersFromOpPtr 1 pos state
  let ptr_out = ptrs !! 0
  let value = trace ("+++ Setting input to 1 for ptr " ++ show ptr_out) 1 -- TODO get actual input!
  updateValue ptr_out value state

-- Output operation
opOutput :: IPtr -> PState -> PState
opOutput pos state = do
  let ptrs = getPointersFromOpPtr 1 pos state
  let ptr_out = ptrs !! 0
  trace ("+++ Value at pointer " ++ (show ptr_out) ++ " is " ++ (show (state !! ptr_out))) state

-- Get a new state with a single uipdated value
updateValue :: IPtr -> Int -> PState -> PState
updateValue pos value state = do
  let (as, bs) = splitAt pos state
  as ++ [value] ++ tail bs

-- Get parameter pointers from instruction pointer
getPointersFromOpPtr :: Int -> IPtr -> PState -> [IPtr]
getPointersFromOpPtr num pos state = do
  let flags = div (state !! pos) 100
  getPointers flags num (pos + 1) state

-- Get pointer values from flags and position of first parameter
getPointers :: Int -> Int -> IPtr -> PState -> [IPtr]
getPointers _ 0 _ _ = []
getPointers flags num pos state = do
  let ptr = case rem flags 10 of
        0 -> state !! pos
        _ -> pos
  ptr : (getPointers (div flags 10) (num - 1) (pos + 1) state)
