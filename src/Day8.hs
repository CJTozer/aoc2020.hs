{-# LANGUAGE ScopedTypeVariables #-}

module Day8 where

import Data.List
import Data.Ord
import Debug.Trace

day8 :: IO ()
day8 = do
  putStrLn "day8 start"
  contents <- readFile "data/day8"
  let ls = layers contents 25 6
  let choice = head (sortBy (\x y -> compare (numZeros x) (numZeros y)) ls)
  print $ show $ (numOnes choice) * (numTwos choice)
  putStrLn $ raster 25 $ resolveImage ls
  putStrLn $ raster 25 $ pixelart $ resolveImage ls
  putStrLn "day8 end"

layers :: String -> Int -> Int -> [String]
layers "" _ _ = []
layers s x y = do
  let (l, ls) = splitAt (x * y) s
  [l] ++ layers ls x y

resolveImage :: [String] -> String
resolveImage (l:ls) = resolveRemainingLayers l ls

resolveRemainingLayers :: String -> [String] -> String
resolveRemainingLayers done [] = done
resolveRemainingLayers done (t:todos) = resolveRemainingLayers (combineLayers done t) todos

pixelart :: String -> String
pixelart "" = ""
pixelart s = map convertPx s

convertPx :: Char -> Char
convertPx '0' = ' '
convertPx '1' = '█'
convertPx _ = '?'

combineLayers :: String -> String -> String
combineLayers "" _ = ""
combineLayers upper lower = do
  let u:us = upper
  let l:ls = lower
  (combinePixels u l) : (combineLayers us ls)

combinePixels :: Char -> Char -> Char
combinePixels upper lower = case upper of
  '2' -> lower
  _ -> upper

raster :: Int -> String -> String
raster _ "" = ""
raster n s = (take n s) ++ "\n" ++ raster n (drop n s)

numXs :: Char -> String -> Int
numXs x xs = length (filter (== x) xs)
numZeros xs = numXs '0' xs
numOnes xs = numXs '1' xs
numTwos xs = numXs '2' xs
