{-# LANGUAGE ScopedTypeVariables #-}

module Day16 (day16) where

import Data.List (any, isInfixOf)
import Data.List.Split (splitOn)
import Debug.Trace

type Range = (Int, Int)
data Rule = Rule {name :: String, ranges :: [Range]} deriving (Show)

day16 :: IO ()
day16 = do
  putStrLn "day16 start"
  contents <- readFile "data/day16"
  let (rules, my_ticket, other_tickets) = parse contents
  print . show $ sum $ collectInvalid other_tickets rules
  let good_tickets = dropInvalid other_tickets rules
  print . show $ (length other_tickets, length good_tickets)
  let valid_positions = validPositions good_tickets rules
  putStrLn "day16 end"

parse :: String -> ([Rule], [Int], [[Int]])
parse s = (rules, my_ticket, other_tickets)
 where
  rules = parseRules rules_part
  my_ticket = parseTicket my_ticket_part
  other_tickets = map parseTicket other_tickets_part
  rules_part = takeWhile (isInfixOf ":") ls
  my_ticket_part = getMyTicket ls
  other_tickets_part = getOtherTickets ls
  ls = lines s

parseRules :: [String] -> [Rule]
parseRules = map parseRule
 where
  parseRule r =
    Rule
      { name = rule_name
      , ranges = rule_ranges
      }
   where
    rule_name = takeWhile (/= ':') r
    rule_ranges = rangesFrom (drop (length rule_name + 2) r)
     where
      rangesFrom r = map rangeFrom (splitOn " or " r)
       where
        rangeFrom r = (l, u) :: (Int, Int)
         where
          (l : u : _) = map read (splitOn "-" r)

parseTicket :: String -> [Int]
parseTicket s = map read (splitOn "," s)

getMyTicket :: [String] -> String
getMyTicket ls = dropWhile (\x -> not ("your ticket:" `isInfixOf` x)) ls !! 1

getOtherTickets :: [String] -> [String]
getOtherTickets ls = tail $ dropWhile (\x -> not ("nearby tickets:" `isInfixOf` x)) ls

collectInvalid :: [[Int]] -> [Rule] -> [Int]
collectInvalid tickets rules =
  filter (`isInvalid` rules) [x | xs <- tickets, x <- xs]

dropInvalid :: [[Int]] -> [Rule] -> [[Int]]
dropInvalid tickets rules =
  filter (`allValid` rules) tickets

allValid :: [Int] -> [Rule] -> Bool
allValid t rules = all (`isValid` rules) t

isInvalid :: Int -> [Rule] -> Bool
isInvalid val rs = not (isValid val rs)

isValid :: Int -> [Rule] -> Bool
isValid _ [] = False
isValid val (r : rs) = valid || isValid val rs
 where
  valid = any (isInRange val) (ranges r)

isInRange :: Int -> (Int, Int) -> Bool
isInRange val (lower, upper) = val >= lower && val <= upper

validPositions :: [[Int]] -> [Rule] -> [[Int]]
validPositions tickets = map (validPositions' tickets)

validPositions' :: [[Int]] -> Rule -> [Int]
validPositions' tickets rule = map snd $ filter fst $ zip valids [0 ..]
 where
  valids = validPositions'' tickets rule

validPositions'' :: [[Int]] -> Rule -> [Bool]
validPositions'' tickets rule =
  if null (head tickets)
    then []
    else all ((`isValid` [rule]) . head) tickets : validPositions'' (map tail tickets) rule
