module Main where

import System.Environment (getArgs)
import System.Exit hiding (die)

main :: IO ()
main = getArgs >>= parse

parse [] = usage >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [file] = do
    putStrLn "Hello"


usage   = putStrLn "Usage: hwk [-hv] <expr>"
version = putStrLn "hwk - Haskell based AWK replacement v0.1"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)
