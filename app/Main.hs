module Main where

import System.Environment
import Day1
import Day2
import Day3
import Day4
import Day5

main :: IO ()
main = getArgs >>= runTest

runTest :: [String] -> IO ()
runTest ["1"] = day1
runTest ["2"] = day2
runTest ["3"] = day3
runTest ["4"] = day4
runTest ["5"] = day5
runTest _ = putStrLn "Please provide the scenario to run"
