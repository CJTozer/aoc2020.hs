{-# LANGUAGE ScopedTypeVariables #-}

module Day7 (
  day7,
  runIntCodeWithInputs,
  chainAmps,
  bestAmplification
  ) where

import Data.List
import Data.List.Split
import Debug.Trace

day7 :: IO ()
day7 = do
  putStrLn "day7 start"
  contents <- readFile "data/day7"
  let best = bestAmplification contents
  print (show best)
  putStrLn "day7 end"

type IPtr = Int -- Instruction Pointer
type PState = ([Int], IOValues) -- Program state
type IOValues = ([Int], [Int]) -- Program inputs & outputs

bestAmplification :: String -> Int
bestAmplification program = do
  last $ sort $ [ chainAmps program phases 0 | phases <- permutations [0,1,2,3,4] ]

chainAmps :: String -> [Int] -> Int -> Int
chainAmps program [x] amp_input = ampOutput program [x, amp_input]
chainAmps program phases amp_input = do
  let phase:rem_ps = phases
  let output = ampOutput program [phase, amp_input]
  chainAmps program rem_ps output

ampOutput :: String -> [Int] -> Int
ampOutput program inputs = do
  let state = runIntCodeWithInputs program inputs
  let (_, (_, outs)) = state
  case outs of
    [x] -> x
    _ -> trace ("Unexpected output " ++ (show outs) ++ " from amp with inputs " ++ (show inputs)) 0

runIntCodeWithInputs :: String -> [Int] -> PState
runIntCodeWithInputs s inputs = do
  let ints = map read $ splitOn "," s
  runIntCodeFrom 0 (ints, (inputs, []))

runIntCodeFrom :: IPtr -> PState -> PState
runIntCodeFrom pos state = do
  let (ints, _) = state
  let opcode = ints !! pos
  case rem opcode 100 of
    1 -> runIntCodeFrom (pos + 4) $ opAdd pos state
    2 -> runIntCodeFrom (pos + 4) $ opMultiply pos state
    3 -> runIntCodeFrom (pos + 2) $ opInput pos state
    4 -> runIntCodeFrom (pos + 2) $ opOutput pos state
    5 -> do
      let new_pos = opJumpIfTrue pos state
      runIntCodeFrom new_pos state
    6 -> do
      let new_pos = opJumpIfFalse pos state
      runIntCodeFrom new_pos state
    7 -> runIntCodeFrom (pos + 4) $ opLessThan pos state
    8 -> runIntCodeFrom (pos + 4) $ opEquals pos state
    99 -> state
    _ -> trace ("!!! Unexpected op code " ++ (show opcode) ++ " at pointer " ++ (show pos)) undefined

-- Addition operation
opAdd :: IPtr -> PState -> PState
opAdd = opMath (+)

-- Multiplication operation
opMultiply :: IPtr -> PState -> PState
opMultiply = opMath (*)

-- Generic maths
opMath :: (Int -> Int -> Int) -> IPtr -> PState -> PState
opMath op pos state = do
  let (ints, ios) = state
  let ptrs = getPointersFromOpPtr 3 pos ints
  let ptr_a = ptrs !! 0
  let ptr_b = ptrs !! 1
  let ptr_out = ptrs !! 2

  let a = ints !! ptr_a
  let b = ints !! ptr_b
  (updateValue ptr_out (op a b) ints, ios)

-- Input operation
-- Cheating for now by knowing the input should be 1
opInput :: IPtr -> PState -> PState
opInput pos state = do
  let (ints, (ins, outs)) = state
  let ptrs = getPointersFromOpPtr 1 pos ints
  let ptr_out = ptrs !! 0
  let value:rem_ins = ins
  -- let new_ios = trace ("+++ Using input " ++ (show value) ++ " remaining inputs: " ++ (show rem_ins)) (rem_ins, outs)
  let new_ios = (rem_ins, outs)
  (updateValue ptr_out value ints, new_ios)

-- Output operation
opOutput :: IPtr -> PState -> PState
opOutput pos state = do
  let (ints, (ins, outs)) = state
  let ptrs = getPointersFromOpPtr 1 pos ints
  let ptr_out = ptrs !! 0
  let new_outs = outs ++ [(ints !! ptr_out)]
  -- trace ("+++ Outputs now: " ++ (show new_outs)) (ints, (ins, new_outs))
  (ints, (ins, new_outs))

-- Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
opJumpIfTrue :: IPtr -> PState -> IPtr
opJumpIfTrue pos state = do
  let (ints, _) = state
  let ptrs = getPointersFromOpPtr 2 pos ints
  let ptr_check = ptrs !! 0
  let ptr_new_pos = ptrs !! 1
  case ints !! ptr_check of
    0 -> pos + 3
    _ -> ints !! ptr_new_pos

-- Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
opJumpIfFalse :: IPtr -> PState -> IPtr
opJumpIfFalse pos state = do
  let (ints, _) = state
  let ptrs = getPointersFromOpPtr 2 pos ints
  let ptr_check = ptrs !! 0
  let ptr_new_pos = ptrs !! 1
  case ints !! ptr_check of
    0 -> ints !! ptr_new_pos
    _ -> pos + 3

-- Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
opLessThan :: IPtr -> PState -> PState
opLessThan pos state = do
  let (ints, ios) = state
  let ptrs = getPointersFromOpPtr 3 pos ints
  let ptr_a = ptrs !! 0
  let ptr_b = ptrs !! 1
  let ptr_out = ptrs !! 2

  -- Get the values to compare
  let a = ints !! ptr_a
  let b = ints !! ptr_b
  case a < b of
    True -> (updateValue ptr_out 1 ints, ios)
    False -> (updateValue ptr_out 0 ints, ios)

-- Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
opEquals :: IPtr -> PState -> PState
opEquals pos state = do
  let (ints, ios) = state
  let ptrs = getPointersFromOpPtr 3 pos ints
  let ptr_a = ptrs !! 0
  let ptr_b = ptrs !! 1
  let ptr_out = ptrs !! 2

  -- Get the values to compare
  let a = ints !! ptr_a
  let b = ints !! ptr_b
  case a == b of
    True -> (updateValue ptr_out 1 ints, ios)
    False -> (updateValue ptr_out 0 ints, ios)

-- Get a new state with a single uipdated value
updateValue :: IPtr -> Int -> [Int] -> [Int]
updateValue pos value ints = do
  let (as, bs) = splitAt pos ints
  as ++ [value] ++ tail bs

-- Get parameter pointers from instruction pointer
getPointersFromOpPtr :: Int -> IPtr -> [Int] -> [IPtr]
getPointersFromOpPtr num pos ints = do
  let flags = div (ints !! pos) 100
  getPointers flags num (pos + 1) ints

-- Get pointer values from flags and position of first parameter
getPointers :: Int -> Int -> IPtr -> [Int] -> [IPtr]
getPointers _ 0 _ _ = []
getPointers flags num pos ints = do
  let ptr = case rem flags 10 of
        0 -> ints !! pos
        _ -> pos
  ptr : (getPointers (div flags 10) (num - 1) (pos + 1) ints)
