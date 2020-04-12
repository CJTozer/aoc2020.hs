{-# LANGUAGE ScopedTypeVariables #-}

module Day4 where

day4 :: IO ()
day4 = do
  putStrLn "day4 start"
  print (show (length validPasswords))
  putStrLn "day4 end"

validPasswords :: [String]
validPasswords =
  filter isValid getCandidates

getCandidates :: [String]
getCandidates = map show [158126..624574]

isValid :: String -> Bool
isValid s =
  and [
    allDigitsAscending s,
    containsExactlyTwoRepeats s
      ]

allDigitsAscending :: String -> Bool
allDigitsAscending [_] = True
allDigitsAscending s =
  and [
    (head s) <= head (tail s),
    allDigitsAscending $ tail s
      ]

-- No longer needed after rule update for part 2
containsRepeated :: String -> Bool
containsRepeated [] = False
containsRepeated [_] = False
containsRepeated s =
  or [
    (head s) == (s !! 1),
    containsRepeated $ tail s
     ]

containsExactlyTwoRepeats :: String -> Bool
containsExactlyTwoRepeats [] = False
containsExactlyTwoRepeats [_] = False
containsExactlyTwoRepeats [a, b] = a == b
containsExactlyTwoRepeats s = do
  let n = numRepeats (head s) s
  case n of
    2 -> True
    _ -> containsExactlyTwoRepeats (drop n s)

numRepeats :: Char -> String -> Int
numRepeats _ "" = 0
numRepeats c s =
  if c == head s
    then 1 + numRepeats c (tail s)
    else 0
