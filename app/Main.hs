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
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21

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
runTest ["13"] = day13
runTest ["14"] = day14
runTest ["15"] = day15
runTest ["16"] = day16
runTest ["17"] = day17
runTest ["18"] = day18
runTest ["19"] = day19
runTest ["20"] = day20
runTest ["21"] = day21
runTest _ = putStrLn "Please provide the scenario to run"
