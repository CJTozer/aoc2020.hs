{-# LANGUAGE ScopedTypeVariables #-}

module Day25 (day25) where

import Data.List.Split
import Debug.Trace

cardPub = 8987316
doorPub = 14681524

day25 :: IO ()
day25 = do
  putStrLn "day25 start"
  let card_loops = findLoopSize 0 1 cardPub 7
  let door_loops = findLoopSize 0 1 doorPub 7
  let enc_key = nLoops door_loops 1 cardPub
  let enc_key' = nLoops card_loops 1 doorPub
  print . show $ (enc_key, enc_key')
  putStrLn "day25 end"

findLoopSize :: Int -> Int -> Int -> Int -> Int
findLoopSize loops_done val target subj =
  if val == target
    then loops_done
    else findLoopSize (loops_done + 1) val' target subj
 where
  val' = (val * subj) `mod` 20201227

nLoops :: Int -> Int -> Int -> Int
nLoops 0 val _ = val
nLoops n val subj = nLoops (n - 1) val' subj
 where
  val' = (val * subj) `mod` 20201227