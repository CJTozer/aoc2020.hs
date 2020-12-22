{-# LANGUAGE ScopedTypeVariables #-}

module Day22 (day22) where

import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type GameState = ([Int], [Int])
data GameResult = AWins [Int] | BWins [Int]

day22 :: IO ()
day22 = do
  putStrLn "day22 start"
  contents <- readFile "data/day22"
  print . show $ parse contents
  print . show . playHand . parse $ contents
  print . show . part2 $ contents
  putStrLn "day22 end"

parse :: String -> GameState
parse s = (a, b)
 where
  [a, b] = (map (map read . tail) . splitOn [""] . lines) s

getWinningScore :: [Int] -> Int
getWinningScore = sum . zipWith (*) [1 ..] . reverse

-- Part 1
playHand :: GameState -> Int
playHand ([], winner) = getWinningScore winner
playHand (winner, []) = getWinningScore winner
playHand s = playHand s'
 where
  s' = playNextRound s

playNextRound :: GameState -> GameState
playNextRound (a, b) =
  if head a > head b
    then (tail a ++ [head a, head b], tail b)
    else (tail a, tail b ++ [head b, head a])

-- Part 2

-- Look at the top card of each deck to decide
canRecurse :: GameState -> Bool
canRecurse (a, b) = (head a < length a) && (head b < length b)

playHand' :: GameState -> Set GameState -> GameResult
playHand' (a, []) _ = AWins a
playHand' ([], b) _ = BWins b
playHand' s@(a, b) hits
  -- If we've already hit this position, A wins
  | Set.member s hits = AWins a
  -- If we can recurse, the winner of this round is the winner of the recursion
  | canRecurse s =
    case r_winner of
      AWins _ -> playHand' (tail a ++ [head a, head b], tail b) hits'
      BWins _ -> playHand' (tail a, tail b ++ [head b, head a]) hits'
  -- Otherwise, it's a "normal" hand; highest card wins
  | otherwise = playHand' (playNextRound s) hits'
 where
  -- Winner of recursive game is only evaluated if needed - new game state Set
  r_winner = playHand' (take (head a) (tail a), take (head b) (tail b)) Set.empty
  -- Add this state to the hits
  hits' = Set.insert s hits

part2 :: String -> Int
part2 s = getWinningScore winning_hand
 where
  g = parse s
  winning_hand = case playHand' g Set.empty of
    AWins a -> a
    BWins b -> b