{-# LANGUAGE ScopedTypeVariables #-}

module Day8 (day8) where

import Data.List.Split
import Text.Regex.TDFA
import qualified Data.Set as Set
import Debug.Trace

data Instruction = Nop Int | Jmp Int | Acc Int deriving (Show)

day8 :: IO ()
day8 = do
  putStrLn "day8 start"
  contents <- readFile "data/day8"
  let instructions = parse contents
  print $ show instructions
  print $ show $ programTick (0, 0) instructions
  print $ show $ runUntilLoop (0, 0) instructions
  print $ show $ findSwapThatTerminates instructions
  putStrLn "day8 end"

parse :: String -> [Instruction]
parse s = map parseLine $ lines s

parseLine :: String -> Instruction
parseLine s = do
  -- Capture but ignore "+" so `read` can parse positive and negative numbers
  let (code, _, num_s, _) = (s =~ " \\+?") :: (String, String, String, [String])
  let num = read num_s :: Int
  case code of
    "acc" -> Acc num
    "jmp" -> Jmp num
    "nop" -> Nop num
    -- Good option for explicit error here?

programTick :: (Int, Int) -> [Instruction] -> (Int, Int)
programTick (ptr, acc) ins =
  case ins !! ptr of
    Acc x -> (ptr + 1, acc + x)
    Jmp x -> (ptr + x, acc)
    Nop x -> (ptr + 1, acc)

runUntilLoop :: (Int, Int) -> [Instruction] -> (Int, Int)
runUntilLoop state ins = runUntilDuplicate state ins Set.empty

-- Returns ptr > last instruction if terminates
runUntilDuplicate :: (Int, Int) -> [Instruction] -> Set.Set(Int) -> (Int, Int)
runUntilDuplicate (ptr, acc) ins hits =
  if Set.member ptr hits || ptr >= length ins
  then
    (ptr, acc)
  else
    runUntilDuplicate (programTick (ptr, acc) ins) ins (Set.insert ptr hits)

-- Useless? We need the acc too
doesItTerminate :: (Int, Int) -> [Instruction] -> (Bool, Int)
doesItTerminate state ins = do
  let (ptr, acc) = runUntilDuplicate state ins Set.empty
  (ptr >= length ins, acc)

runWithNthSwapped :: Int -> [Instruction] -> (Bool, Int)
runWithNthSwapped n ins = do
  doesItTerminate (0, 0) (swapNth n ins)

swapNth :: Int -> [Instruction] -> [Instruction]
swapNth n ins = do
  let h = take n ins
  let (x:t) = drop n ins
  h ++ (swapInstruction x):t

-- Swap Jmp for Nop and vice-versa, leave anything else unchanged
swapInstruction :: Instruction -> Instruction
swapInstruction x =
  case x of
    Nop x -> Jmp x
    Jmp x -> Nop x
    y -> y

findSwapThatTerminates :: [Instruction] -> (Bool, Int)
findSwapThatTerminates ins =
  head $ filter (\(y, _) -> y) $ map (\x -> runWithNthSwapped x ins) [0..(length ins - 1)]
