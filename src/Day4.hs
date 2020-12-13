{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (
  day4
, validByr
) where

import Data.List ( isInfixOf )
import Data.List.Split ( splitOn )
import Text.Regex.TDFA ( (=~) )

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
  validByr s &&
  validIyr s &&
  validEyr s &&
  validHgt s &&
  validHcl s &&
  validEcl s &&
  validPid s

validByr :: String -> Bool
validByr s = do
  -- byr (Birth Year) - four digits; at least 1920 and at most 2002.
  let (_, _, _, submatches) = (s =~ "byr:([0-9]+)") :: (String, String, String, [String])
  not (null submatches) && do
    let year :: Int = read $ head submatches
    year >= 1920 && year <= 2002

validIyr :: String -> Bool
validIyr s = do
  -- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  let (_, _, _, submatches) = (s =~ "iyr:([0-9]+)") :: (String, String, String, [String])
  not (null submatches) && do
    let year :: Int = read $ head submatches
    year >= 2010 && year <= 2020

validEyr :: String -> Bool
validEyr s = do
  -- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  let (_, _, _, submatches) = (s =~ "eyr:([0-9]+)") :: (String, String, String, [String])
  not (null submatches) && do
    let year :: Int = read $ head submatches
    year >= 2020 && year <= 2030

validHgt :: String -> Bool
validHgt s = do
  -- hgt (Height) - a number followed by either cm or in:
  --     If cm, the number must be at least 150 and at most 193.
  --     If in, the number must be at least 59 and at most 76.
  let (_, _, _, submatches) = (s =~ "hgt:([0-9]+)(in|cm)") :: (String, String, String, [String])
  length submatches > 1 && do
    let h :: Int = read $ head submatches
    let unit = submatches !! 1
    (unit == "cm" && h >= 150 && h <= 193) ||
      (unit == "in" && h >= 59 && h <= 76)

validHcl :: String -> Bool
validHcl s = do
  -- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  let (_, _, _, submatches) = (s =~ "hcl:#([0-9a-f]+)") :: (String, String, String, [String])
  not (null submatches) && length (head submatches) == 6

validEcl :: String -> Bool
validEcl s = do
  -- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  let (_, _, _, submatches) = (s =~ "ecl:(amb|blu|brn|gry|grn|hzl|oth)") :: (String, String, String, [String])
  not (null submatches)

validPid :: String -> Bool
validPid s = do
  -- pid (Passport ID) - a nine-digit number, including leading zeroes.
  let (_, _, _, submatches) = (s =~ "pid:([0-9]+)") :: (String, String, String, [String])
  not (null submatches) && length (head submatches) == 9
