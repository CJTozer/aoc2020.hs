{-# LANGUAGE ScopedTypeVariables #-}

module Day7 (
  day7,
  runIntCodeWithInputs,
  chainAmps,
  bestAmplification,
  bestFeedback
  ) where

import Data.List
import Data.List.Split
import Debug.Trace

day7 :: IO ()
day7 = do
  putStrLn "day7 start"
  contents <- readFile "data/day7"
  let best = bestFeedback contents
  print (show best)
  putStrLn "day7 end"

type IPtr = Int -- Instruction Pointer
type PState = ([Int], IOValues) -- Program state
type FullPState = (Bool, IPtr, PState) -- Full program state
type IOValues = ([Int], [Int]) -- Program inputs & outputs

bestFeedback :: String -> Int
bestFeedback program = do
  last $ sort $ [ feedbackAmpsForPhases program phases | phases <- permutations [5,6,7,8,9] ]

feedbackAmpsForPhases :: String -> [Int] -> Int
feedbackAmpsForPhases program phases = do
  let ints = map read $ splitOn "," program

  let amps :: [FullPState] = map (\p -> (False, 0, (ints, ([p], [])))) phases
  let outputs = feedbackAmps amps [0] -- Initial extra input is zero
  -- trace ("For sequence " ++ (show phases) ++ " got output " ++ (show outputs)) head outputs
  head outputs

feedbackAmps :: [FullPState] -> [Int] -> [Int]
feedbackAmps amps next_inputs = do
  let amp:rem_amps = amps
  -- let (complete, _, _) = trace ("Running amp: " ++ (show amp) ++ " with extra inputs " ++ (show next_inputs)) amp
  let (complete, _, _) = amp
  case complete of
    -- Next amp is in completed state, we must be done here
    True -> next_inputs
    -- Run this amp and continue
    False -> do
      let updated_amp = runIntCodeFrom $ updateInputs amp next_inputs
      let (comp, pos, (ints, (inputs, new_output))) = updated_amp
      -- New list has this amp at the end, with outputs cleared
      let new_amps = rem_amps ++ [(comp, pos, (ints, (inputs, [])))]
      feedbackAmps new_amps new_output

updateInputs :: FullPState -> [Int] -> FullPState
updateInputs state extra_inputs = do
  let (comp, pos, (ints, (inputs, outputs))) = state
  (comp, pos, (ints, (inputs ++ extra_inputs, outputs)))

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
  let (_, _, (_, (_, outs))) = state
  case outs of
    [x] -> x
    _ -> trace ("Unexpected output " ++ (show outs) ++ " from amp with inputs " ++ (show inputs)) 0

runIntCodeWithInputs :: String -> [Int] -> FullPState
runIntCodeWithInputs s inputs = do
  let ints = map read $ splitOn "," s
  runIntCodeFrom (False, 0, (ints, (inputs, [])))

runIntCodeFrom :: FullPState -> FullPState
runIntCodeFrom full_state = do
  let (_, pos, state) = full_state
  let (ints, (inputs, _)) = state
  let opcode = ints !! pos
  case rem opcode 100 of
    1 -> runIntCodeFrom (False, (pos + 4), opAdd pos state)
    2 -> runIntCodeFrom (False, (pos + 4), opMultiply pos state)
    3 -> case inputs of
      -- No more inputs, we have to wait
      [] -> full_state
      -- Input availble, continue
      _ -> runIntCodeFrom (False, (pos + 2), opInput pos state)
    4 -> runIntCodeFrom (False, (pos + 2), opOutput pos state)
    5 -> do
      let new_pos = opJumpIfTrue pos state
      runIntCodeFrom (False, new_pos, state)
    6 -> do
      let new_pos = opJumpIfFalse pos state
      runIntCodeFrom (False, new_pos, state)
    7 -> runIntCodeFrom (False, (pos + 4), opLessThan pos state)
    8 -> runIntCodeFrom (False, (pos + 4), opEquals pos state)
    -- Termination
    99 -> (True, pos, state)
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
