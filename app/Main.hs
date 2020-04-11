module Main where

import System.Environment
import Day1

main :: IO ()
main = getArgs >>= runTest

runTest :: [String] -> IO ()
runTest ["1"] = day1
runTest _ = putStrLn "Please provide the scenario to run"
