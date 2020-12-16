{-# LANGUAGE ScopedTypeVariables #-}

module Day16 (day16) where

import Data.List (isInfixOf)
import Data.List.Split
import Debug.Trace

type Range = (Int, Int)
data Rule = Rule {name :: String, ranges :: [Range]} deriving (Show)

day16 :: IO ()
day16 = do
  putStrLn "day16 start"
  contents <- readFile "data/day16"
  print . show $ parse contents
  putStrLn "day16 end"

parse :: String -> ([Rule], [Int], [[Int]])
parse s = (rules, my_ticket, other_tickets)
 where
  rules = parseRules rules_part
  my_ticket = parseTicket my_ticket_part
  other_tickets = map parseTicket other_tickets_part
  rules_part = takeWhile (isInfixOf ":") (lines s)
  my_ticket_part = "1,2,3,4,5" -- TODO
  other_tickets_part = ["9,8,7,6,5"] -- TODO

parseRules :: [String] -> [Rule]
parseRules = map parseRule
 where
  parseRule r =
    Rule
      { name = rule_name
      , ranges = rule_ranges
      }
   where
    rule_name = "test" -- TODO
    rule_ranges = [(0, 1)] -- TODO

parseTicket :: String -> [Int]
parseTicket s = trace (show (s, splitOn "," s)) map read (splitOn "," s)