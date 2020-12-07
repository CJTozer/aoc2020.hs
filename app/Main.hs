module Main where

import System.Environment
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6

main :: IO ()
main = getArgs >>= runTest

runTest :: [String] -> IO ()
runTest ["1"] = day1
runTest ["2"] = day2
runTest ["3"] = day3
runTest ["4"] = day4
runTest ["5"] = day5
runTest ["6"] = day6
runTest _ = putStrLn "Please provide the scenario to run"
