{-# LANGUAGE ScopedTypeVariables #-}

module Day1 where

day1 :: IO ()
day1 = do
  putStrLn "day1 start"
  contents <- readFile "data/day1"
  let ints :: [Int] = map read $ lines contents
  print $ show $ fuelNeeded ints
  putStrLn "day1 end"

fuelNeeded :: [Int] -> Int
fuelNeeded xs = sum $ map fuelForModuleAndFuel xs

fuelForModule :: Int -> Int
fuelForModule x
  | x <= 6 = 0
  | otherwise = x `div` 3 - 2

fuelForModuleAndFuel :: Int -> Int
fuelForModuleAndFuel 0 = 0
fuelForModuleAndFuel x = do
  let fuel = fuelForModule x
  fuel + fuelForModuleAndFuel fuel
