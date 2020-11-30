{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (
  day4
, valid_byr
  ) where

import Data.List
import Data.List.Split
import Text.Regex.TDFA
import Debug.Trace

day4 :: IO ()
day4 = do
  putStrLn "day4 start"
  contents <- readFile "data/day4"
  let entries = splitOn "\n\n" contents
  putStrLn $ head entries
  print $ show $ length (filter validEntry entries)
  print $ show $ length (filter validEntry' entries)
  putStrLn "day4 end"

validEntry :: String -> Bool
validEntry s =
  -- Need: byr, iyr, eyr, hgt, hcl, ecl, pid
  -- Optional: cid
  isInfixOf "byr:" s &&
  isInfixOf "iyr:" s &&
  isInfixOf "eyr:" s &&
  isInfixOf "hgt:" s &&
  isInfixOf "hcl:" s &&
  isInfixOf "ecl:" s &&
  isInfixOf "pid:" s

validEntry' :: String -> Bool
validEntry' s =
  valid_byr s &&
  valid_iyr s &&
  valid_eyr s &&
  valid_hgt s &&
  valid_hcl s &&
  valid_ecl s &&
  valid_pid s

valid_byr :: String -> Bool
valid_byr s = do
  -- byr (Birth Year) - four digits; at least 1920 and at most 2002.
  let (before, match, after, submatches) = (s =~ "byr:([0-9]+)") :: (String, String, String, [String])
  length submatches > 0 && do
    let year :: Int = read $ submatches !! 0
    year >= 1920 && year <= 2002

valid_iyr :: String -> Bool
valid_iyr s = do
  -- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  let (before, match, after, submatches) = (s =~ "iyr:([0-9]+)") :: (String, String, String, [String])
  length submatches > 0 && do
    let year :: Int = read $ submatches !! 0
    year >= 2010 && year <= 2020

valid_eyr :: String -> Bool
valid_eyr s = do
  -- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  let (before, match, after, submatches) = (s =~ "eyr:([0-9]+)") :: (String, String, String, [String])
  length submatches > 0 && do
    let year :: Int = read $ submatches !! 0
    year >= 2020 && year <= 2030

valid_hgt :: String -> Bool
valid_hgt s = do
  -- hgt (Height) - a number followed by either cm or in:
  --     If cm, the number must be at least 150 and at most 193.
  --     If in, the number must be at least 59 and at most 76.
  let (before, match, after, submatches) = (s =~ "hgt:([0-9]+)(in|cm)") :: (String, String, String, [String])
  length submatches > 1 && do
    let h :: Int = read $ submatches !! 0
    let unit = submatches !! 1
    (unit == "cm" && h >= 150 && h <= 193) ||
      (unit == "in" && h >= 59 && h <= 76)

valid_hcl :: String -> Bool
valid_hcl s = do
  -- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  let (before, match, after, submatches) = (s =~ "hcl:#([0-9a-f]+)") :: (String, String, String, [String])
  length submatches > 0 && length (submatches !! 0) == 6

valid_ecl :: String -> Bool
valid_ecl s = do
  -- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  let (before, match, after, submatches) = (s =~ "ecl:(amb|blu|brn|gry|grn|hzl|oth)") :: (String, String, String, [String])
  length submatches > 0

valid_pid :: String -> Bool
valid_pid s = do
  -- pid (Passport ID) - a nine-digit number, including leading zeroes.
  let (before, match, after, submatches) = (s =~ "pid:([0-9]+)") :: (String, String, String, [String])
  length submatches > 0 && length (submatches !! 0) == 9
