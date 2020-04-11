module Main where

import System.Environment
import Lib

main :: IO ()
main = getArgs >>= runTest

runTest :: [String] -> IO ()
runTest ["1"] = someFunc
runTest _ = putStrLn "Please provide the scenario to run"
