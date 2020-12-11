module Main where

import System.Environment
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12

main :: IO ()
main = getArgs >>= runTest

runTest :: [String] -> IO ()
runTest ["1"] = day1
runTest ["2"] = day2
runTest ["3"] = day3
runTest ["4"] = day4
runTest ["5"] = day5
runTest ["6"] = day6
runTest ["7"] = day7
runTest ["8"] = day8
runTest ["9"] = day9
runTest ["10"] = day10
runTest ["11"] = day11
runTest ["12"] = day12
runTest _ = putStrLn "Please provide the scenario to run"
