{-# LANGUAGE ScopedTypeVariables #-}

module Day4 where

day4 :: IO ()
day4 = do
  putStrLn "day4 start"
  print (show (length validPasswords))
  putStrLn "day4 end"

validPasswords :: [String]
validPasswords = []

allDigitsAscending :: String -> Bool
allDigitsAscending [_] = True
allDigitsAscending s =
  and [
    (head s) <= head (tail s),
    allDigitsAscending $ tail s
  ]
