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
    containsRepeated s
      ]

allDigitsAscending :: String -> Bool
allDigitsAscending [_] = True
allDigitsAscending s =
  and [
    (head s) <= head (tail s),
    allDigitsAscending $ tail s
      ]

containsRepeated :: String -> Bool
containsRepeated [] = False
containsRepeated [_] = False
containsRepeated s =
  or [
    (head s) == (s !! 1),
    containsRepeated $ tail s
     ]

containsThreeRepeats :: String -> Bool
containsThreeRepeats [] = False
containsThreeRepeats [_] = False
containsThreeRepeats [_, _] = False
containsThreeRepeats s =
  or [
    and [
      (head s) == (s !! 1),
      (head s) == (s !! 2)
        ],
    containsThreeRepeats $ tail s
     ]
