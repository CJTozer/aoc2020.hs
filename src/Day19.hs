{-# LANGUAGE ScopedTypeVariables #-}

module Day19 (
  day19,
  regexFromRule,
) where

import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Text.Regex.TDFA ((=~))

-- Part 2 done by replacing:
-- > 8: 42
-- > 11: 42 31
-- with:
-- > 8: 42 | 42 8
-- > 11: 42 31 | 42 11 31
--
-- Done by replacing the rules with
-- > 8: 42 | 42 42 | 42 42 42 | ...
-- > 11: 42 31 | 42 42 31 31 | 42 42 31 31 | ...
-- etc., until getting a consistent result
-- ...rather than trying (for 11) to get the regex to capture only when the 42s and 31s repeat the same number of times...

day19 :: IO ()
day19 = do
  putStrLn "day19 start"
  contents <- readFile "data/day19"
  let (rules : messages : _) = splitOn [""] . lines $ contents
  print . show $ countValidMessages rules messages
  putStrLn "day19 end"

countValidMessages :: [String] -> [String] -> Int
countValidMessages rules = length . filter (isValidMessage rules_regex)
 where
  rules_regex = regexFromRules rules

regexFromRules :: [String] -> String
regexFromRules rules = "^" ++ regexFromRule rule_0 rules ++ "$"
 where
  rule_0 = findRule "0" rules

regexFromRule :: String -> [String] -> String
regexFromRule rule rules =
  if "|" `isInfixOf` rule
    then combineOptionals (splitOn " | " rule) rules
    else case rule of
      "\"a\"" -> "a"
      "\"b\"" -> "b"
      _ -> combineSequence (splitOn " " rule) rules

regexFromRuleId :: String -> [String] -> String
regexFromRuleId id rules = regexFromRule (findRule id rules) rules

combineOptionals :: [String] -> [String] -> String
combineOptionals parts rules =
  "(" ++ intercalate "|" (map (`regexFromRule` rules) parts) ++ ")"

combineSequence :: [String] -> [String] -> String
combineSequence parts rules = concatMap (`regexFromRuleId` rules) parts

findRule :: String -> [String] -> String
findRule id = drop (length id + 2) . head . filter ((id ++ ": ") `isPrefixOf`)

isValidMessage :: String -> String -> Bool
isValidMessage regex s = s =~ regex