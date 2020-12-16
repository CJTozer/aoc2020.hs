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
