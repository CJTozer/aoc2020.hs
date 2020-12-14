{-# LANGUAGE ScopedTypeVariables #-}

module Day14 (day14, maskFromString, Instruction (Mask)) where

import Data.Bits (Bits (complement), (.&.), (.|.))
import Data.Char (digitToInt)
import qualified Data.IntMap.Strict as IntMap
import Data.List (isPrefixOf, stripPrefix)
import Data.List.Split (splitOn)
import Numeric (readInt)

-- Mask instruction needs two Ints - bits to set and bits to unset
-- Mem instruction is an address and a value
data Instruction = Mask Int Int | Mem Int Int deriving (Show, Eq)

day14 :: IO ()
day14 = do
  putStrLn "day14 start"
  contents <- readFile "data/day14"
  let instructions = parseInput . lines $ contents
  print . take 2 $ instructions
  print . part1 $ instructions
  putStrLn "day14 end"

-- ## RUNNING THE PROGRAM
part1 :: [Instruction] -> Int
part1 ins = IntMap.foldr (+) 0 mem_map
 where
  mem_map = runInstructions ins IntMap.empty (0, 0)

runInstructions :: [Instruction] -> IntMap.IntMap Int -> (Int, Int) -> IntMap.IntMap Int
runInstructions [] x _ = x
runInstructions (ii : iis) mem_map mask =
  case ii of
    Mask set unset -> runInstructions iis mem_map (set, unset)
    Mem addr val -> runInstructions iis (IntMap.insert addr (masked val mask) mem_map) mask

-- Mask the given value
masked :: Int -> (Int, Int) -> Int
masked val (set, unset) = set .|. (val .&. complement unset)

-- ## PARSING
parseInput :: [String] -> [Instruction]
parseInput = map parseLine

parseLine :: String -> Instruction
parseLine s =
  if "mask = " `isPrefixOf` s
    then parseMask s
    else parseMem s

parseMask :: String -> Instruction
parseMask s = case stripPrefix "mask = " s of
  Nothing -> error "Logic error!"
  Just m -> maskFromString m

maskFromString :: String -> Instruction
maskFromString s = Mask set unset
 where
  set = readBin . convertToMask $ s
  unset = readBin . convertToUnmask $ s

convertToMask :: String -> String
convertToMask = replace 'X' '0'

-- Care needed for order of operations here!
-- 1 -> X, then 0 -> 1, then X -> 0
convertToUnmask :: String -> String
convertToUnmask = replace 'X' '0' . replace '0' '1' . replace '1' 'X'

readBin :: String -> Int
readBin s = fst . head $ readInt 2 (`elem` "01") digitToInt s

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\x -> if a == x then b else x)

parseMem :: String -> Instruction
parseMem s = case stripPrefix "mem[" s of
  Nothing -> error $ "Invalid instruction: " ++ s
  Just s' -> Mem address value
   where
    address = read address_s
    value = read value_s
    (address_s : value_s : _) = splitOn "] = " s'