{-# LANGUAGE ScopedTypeVariables #-}

module Day14 (day14, collectAddresses, Instruction (Mask)) where

import Data.Char (digitToInt)
import qualified Data.IntMap.Strict as IntMap
import Data.List (isPrefixOf, stripPrefix)
import Data.List.Split (splitOn)
import Numeric (readInt)
import Text.Printf (printf)

-- Mask instruction needs two Ints - bits to set and bits to unset
-- Mem instruction is an address and a value
data Instruction = Mask String | Mem Int Int deriving (Show, Eq)

day14 :: IO ()
day14 = do
  putStrLn "day14 start"
  contents <- readFile "data/day14"
  let instructions = parseInput . lines $ contents
  print . take 2 $ instructions
  print . part2 $ instructions
  putStrLn "day14 end"

-- ## RUNNING THE PROGRAM
part2 :: [Instruction] -> Int
part2 ins = IntMap.foldr (+) 0 mem_map
 where
  mem_map = runInstructions ins IntMap.empty null_mask
  null_mask = "000000000000000000000000000000000000"

runInstructions :: [Instruction] -> IntMap.IntMap Int -> String -> IntMap.IntMap Int
runInstructions [] x _ = x
runInstructions (ii : iis) mem_map mask =
  case ii of
    Mask m -> runInstructions iis mem_map m
    Mem addr val -> runInstructions iis (updateMem mem_map addr mask val) mask

-- Set the value in all memory addresses indicated by the mask
updateMem :: IntMap.IntMap Int -> Int -> String -> Int -> IntMap.IntMap Int
updateMem mem_map addr mask = updateMem' mem_map (allAddresses addr mask)

updateMem' :: IntMap.IntMap Int -> [Int] -> Int -> IntMap.IntMap Int
updateMem' mem_map [] _ = mem_map
updateMem' mem_map (addr : addrs) val =
  updateMem' mem_map' addrs val
 where
  mem_map' = IntMap.insert addr val mem_map

-- Get all memory addresses for address and mask
allAddresses :: Int -> String -> [Int]
allAddresses base mask = do
  -- Convert base to a string
  let base_s = printf "%036b" base :: String
  map readBin $ collectAddresses base_s mask

-- collect addresses bit by bit
collectAddresses :: String -> String -> [String]
collectAddresses [] _ = [""]
collectAddresses (b : bs) (m : ms) =
  case m of
    '0' -> map (b :) rem_addresses
    '1' -> map ('1' :) rem_addresses
    'X' -> ["0", "1"] >>= (\x -> map (x ++) rem_addresses)
 where
  rem_addresses = collectAddresses bs ms

-- ## PARSING THE INPUT
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
  Just m -> Mask m

readBin :: String -> Int
readBin s = fst . head $ readInt 2 (`elem` "01") digitToInt s

parseMem :: String -> Instruction
parseMem s = case stripPrefix "mem[" s of
  Nothing -> error $ "Invalid instruction: " ++ s
  Just s' -> Mem address value
   where
    address = read address_s
    value = read value_s
    (address_s : value_s : _) = splitOn "] = " s'