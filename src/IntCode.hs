{-# LANGUAGE ScopedTypeVariables #-}

module IntCode (
  runIntCodeWithInputs,
  getOutputs
  ) where

import Data.List.Split
import Debug.Trace

type IPtr = Int -- Instruction Pointer
type PState = ([Int], IOValues) -- Program state
type FullPState = (Bool, IPtr, IPtr, PState) -- Full program state
type IOValues = ([Int], [Int]) -- Program inputs & outputs

getOutputs :: FullPState -> [Int]
getOutputs (_, _, _, (_, (_, outputs))) = outputs

runIntCodeWithInputs :: String -> [Int] -> FullPState
runIntCodeWithInputs s inputs = do
  let ints = map read $ splitOn "," s
  runIntCodeFrom (False, 0, 0, (ints, (inputs, [])))

runIntCodeFrom :: FullPState -> FullPState
runIntCodeFrom state = do
  let (_, pos, base, (ints, (ins, outs))) = state
  let opcode = getValue ints pos
  case rem opcode 100 of
    1 -> runIntCodeFrom (opAdd state)
    2 -> runIntCodeFrom (opMultiply state)
    3 -> case ins of
      -- No more inputs, we have to wait
      [] -> state
      -- Input availble, continue
      _ -> runIntCodeFrom (opInput state)
    4 -> runIntCodeFrom (opOutput state)
    5 -> runIntCodeFrom (opJumpIfTrue state)
    6 -> runIntCodeFrom (opJumpIfFalse state)
    7 -> runIntCodeFrom (opLessThan state)
    8 -> runIntCodeFrom (opEquals state)
    9 -> runIntCodeFrom (opAdjustBase state)
    -- Termination
    99 -> (True, pos, base, (ints, (ins, outs)))
    _ -> trace ("!!! Unexpected op code " ++ (show opcode) ++ " at pointer " ++ (show pos)) undefined

getValue :: [Int] -> IPtr -> Int
getValue ints pos = if pos > length ints
  then 0
  else ints !! pos

-- Addition operation
opAdd :: FullPState -> FullPState
opAdd = opMath (+)

-- Multiplication operation
opMultiply :: FullPState -> FullPState
opMultiply = opMath (*)

-- Generic maths
opMath :: (Int -> Int -> Int) -> FullPState -> FullPState
opMath op (complete, pos, base, (ints, ios)) = do
  let ptrs = getPointersFromOpPtr 3 pos base ints
  let ptr_a = ptrs !! 0
  let ptr_b = ptrs !! 1
  let ptr_out = ptrs !! 2

  let a = getValue ints ptr_a
  let b = getValue ints ptr_b
  (complete, pos + 4, base, (updateValue ptr_out (op a b) ints, ios))

-- Input operation
opInput :: FullPState -> FullPState
opInput (complete, pos, base, (ints, (ins, outs))) = do
  let ptrs = getPointersFromOpPtr 1 pos base ints
  let ptr_out = ptrs !! 0
  let value:rem_ins = ins
  -- let new_ios = trace ("+++ Using input " ++ (show value) ++ " remaining inputs: " ++ (show rem_ins)) (rem_ins, outs)
  let new_ios = (rem_ins, outs)
  (complete, pos + 2, base, (updateValue ptr_out value ints, new_ios))

-- Output operation
opOutput :: FullPState -> FullPState
opOutput (complete, pos, base, (ints, (ins, outs))) = do
  let ptrs = getPointersFromOpPtr 1 pos base ints
  let ptr_out = ptrs !! 0
  let new_outs = outs ++ [(getValue ints ptr_out)]
  -- trace ("+++ Outputs now: " ++ (show new_outs)) (ints, (ins, new_outs))
  (complete, pos + 2, base, (ints, (ins, new_outs)))

-- Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
opJumpIfTrue :: FullPState -> FullPState
opJumpIfTrue (complete, pos, base, (ints, ios)) = do
  let ptrs = getPointersFromOpPtr 2 pos base ints
  let ptr_check = ptrs !! 0
  let ptr_new_pos = ptrs !! 1
  let new_pos = case getValue ints ptr_check of
        0 -> pos + 3
        _ -> getValue ints ptr_new_pos
  (complete, new_pos, base, (ints, ios))

-- Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
opJumpIfFalse :: FullPState -> FullPState
opJumpIfFalse (complete, pos, base, (ints, ios)) = do
  let ptrs = getPointersFromOpPtr 2 pos base ints
  let ptr_check = ptrs !! 0
  let ptr_new_pos = ptrs !! 1
  let new_pos = case getValue ints ptr_check of
        0 -> getValue ints ptr_new_pos
        _ -> pos + 3
  (complete, new_pos, base, (ints, ios))

-- Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
opLessThan :: FullPState -> FullPState
opLessThan (complete, pos, base, (ints, ios)) = do
  let ptrs = getPointersFromOpPtr 3 pos base ints
  let ptr_a = ptrs !! 0
  let ptr_b = ptrs !! 1
  let ptr_out = ptrs !! 2

  -- Get the values to compare
  let a = getValue ints ptr_a
  let b = getValue ints ptr_b
  let new_ints = case a < b of
        True -> updateValue ptr_out 1 ints
        False -> updateValue ptr_out 0 ints
  (complete, pos + 4, base, (new_ints, ios))

-- Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
opEquals :: FullPState -> FullPState
opEquals (complete, pos, base, (ints, ios)) = do
  let ptrs = getPointersFromOpPtr 3 pos base ints
  let ptr_a = ptrs !! 0
  let ptr_b = ptrs !! 1
  let ptr_out = ptrs !! 2

  -- Get the values to compare
  let a = getValue ints ptr_a
  let b = getValue ints ptr_b
  let new_ints = case a == b of
        True -> updateValue ptr_out 1 ints
        False -> updateValue ptr_out 0 ints
  (complete, pos + 4, base, (new_ints, ios))

-- Opcode 9 adjusts the relative base by the value of its only parameter. The relative base increases (or decreases, if the value is negative) by the value of the parameter.
opAdjustBase :: FullPState -> FullPState
opAdjustBase state = do
  let (complete, pos, base, (ints, ios)) = state
  let ptr = head (getPointersFromOpPtr 1 pos base ints)
  (complete, pos + 2, base + (getValue ints ptr), (ints, ios))

-- Get a new state with a single uipdated value
updateValue :: IPtr -> Int -> [Int] -> [Int]
updateValue pos value ints = do
  let new_space = if (pos > length ints)
        then ints ++ replicate (pos - (length ints) + 1) 0
        else ints
  let (as, bs) = splitAt pos new_space
  as ++ [value] ++ case bs of
    [] ->[]
    _ -> tail bs

-- Get parameter pointers from instruction pointer
getPointersFromOpPtr :: Int -> IPtr -> IPtr -> [Int] -> [IPtr]
getPointersFromOpPtr num pos base ints = do
  let flags = div (getValue ints pos) 100
  getPointers flags num (pos + 1) base ints

-- Get pointer values from flags and position of first parameter
getPointers :: Int -> Int -> IPtr -> IPtr -> [Int] -> [IPtr]
getPointers _ 0 _ _ _ = []
getPointers flags num pos base ints = do
  let ptr = case rem flags 10 of
        0 -> getValue ints pos
        1 -> pos
        2 -> base + (getValue ints pos)
        x -> trace ("Unexpected instruction " ++ (show (getValue ints ptr)) ++ " at address " ++ (show pos)) undefined
  ptr : (getPointers (div flags 10) (num - 1) (pos + 1) base ints)
