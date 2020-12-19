{-# LANGUAGE ScopedTypeVariables #-}

module Day18 (
  day18,
  takeUntilClosingBracket,
  doCalc,
  parseExpression,
  evalExpression,
  Part (Value),
) where

import Data.Char (isAlphaNum)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Debug.Trace ()

day18 :: IO ()
day18 = do
  putStrLn "day18 start"
  contents <- readFile "data/day18"
  print . show . sum . map doCalc . lines $ contents
  print . show . sum . map (getValue . evalExpression . parseExpression) . lines $ contents
  putStrLn "day18 end"

doCalc :: String -> Int
doCalc = subCalc 0 (+)

nullOp :: Int -> Int -> Int
nullOp _ _ = error "Trying to evaluate nullOp"

subCalc :: Int -> (Int -> Int -> Int) -> String -> Int
subCalc acc _ "" = acc
subCalc acc op s@(x : xs) =
  case x of
    ' ' -> subCalc acc op xs
    '+' -> subCalc acc (+) xs
    '*' -> subCalc acc (*) xs
    '(' -> subCalc acc' nullOp xs'
     where
      acc' = op acc $ subCalc 0 (+) (init in_brackets) -- Drop final ')' from returned string
      (in_brackets, xs') = takeUntilClosingBracket 1 xs
    _ -> subCalc acc' nullOp s'
     where
      acc' = op acc val
      val = read (takeWhile isAlphaNum s)
      s' = dropWhile isAlphaNum s

takeUntilClosingBracket :: Int -> String -> (String, String)
takeUntilClosingBracket _ "" = ("", "")
takeUntilClosingBracket 0 s = ("", s)
takeUntilClosingBracket n (x : xs) = (x : in_brackets, rem)
 where
  n' = case x of
    '(' -> n + 1
    ')' -> n - 1
    _ -> n
  (in_brackets, rem) = takeUntilClosingBracket n' xs

-- More flexible implementation for part 2
type Expression = [Part]
data Part = Add | Mult | Bracketed Expression | Value Int deriving (Show, Eq)

parseExpression :: String -> Expression
parseExpression s =
  case elemIndex '(' s of
    Nothing -> map parsePart . filter (not . null) . splitOn " " $ s
    Just x ->
      parseExpression (take x s)
        ++ [Bracketed b_exp]
        ++ parseExpression rem
     where
      b_exp = parseExpression (init b_exp_s) -- Drop final ')' from returned string
      (b_exp_s, rem) = takeUntilClosingBracket 1 (drop (x + 1) s)

parsePart :: String -> Part
parsePart s = case s of
  "+" -> Add
  "*" -> Mult
  x -> Value (read x)

getValue :: Part -> Int
getValue (Value x) = x
getValue y = error ("Cannot get value from: " ++ show y)

evalExpression :: Expression -> Part
evalExpression e = case e' of
  [x] -> x
  _ -> error ("Evaluation ended incomplete: " ++ show e')
 where
  e' = evalMults . evalAdds . evalBrackets $ e

evalBrackets :: Expression -> Expression
evalBrackets [] = []
evalBrackets (p : ps) = p' : ps'
 where
  p' = case p of
    Bracketed e -> evalExpression e
    x -> x
  ps' = evalBrackets ps

evalAdds :: Expression -> Expression
evalAdds e = case elemIndex Add e of
  Nothing -> e
  Just x -> evalAdds e'
   where
    e' = take (x - 1) e ++ [Value (a + b)] ++ drop (x + 2) e
    Value a = e !! (x - 1)
    Value b = e !! (x + 1)

evalMults :: Expression -> Expression
evalMults e = case elemIndex Mult e of
  Nothing -> e
  Just x -> evalMults e'
   where
    e' = take (x - 1) e ++ [Value (a * b)] ++ drop (x + 2) e
    Value a = e !! (x - 1)
    Value b = e !! (x + 1)