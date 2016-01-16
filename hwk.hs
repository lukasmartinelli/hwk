module Main where

import System.Environment (getArgs, getEnvironment)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.Posix.Process (executeFile)
import System.Posix.Temp (mkdtemp)
import Data.List (isPrefixOf)

main :: IO ()
main = getArgs >>= parse

parse [] = usage >> die
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["clear"] = do
    envs <- getEnvironment
    putStrLn $ unlines $ envUnsetFunctions envs
    exit
parse ["env", fname, stmt] = do
    putStrLn $ "export " ++ envFunctionPrefix ++ fname ++ "='" ++ stmt ++ "'"
    exit
parse [stmt] = do
    envs <- getEnvironment
    tempDir <- mkdtemp "/tmp/"
    let tempFile = tempDir ++ "/hwk.hs"
    writeFile tempFile $ genModule (envFunctions envs) stmt
    executeFile "runhaskell" True [tempFile] Nothing

genMain fname = "main = getContents >>= \\contents -> " ++
                "mapM_ putStrLn $ " ++ fname ++ " $ lines contents"

genFunction fname stmt = fname ++ " = " ++ stmt
genModule functions stmt = unlines $ functions ++ [genHwkFunction "hwk" stmt, genMain "hwk"]
genHwkFunction fname stmt = unlines [ fname ++ " :: [String] -> [String]"
                                    , genFunction fname stmt
                                    ]

exit    = exitSuccess
die     = exitWith (ExitFailure 1)
envFunctions envs = [envGenFunction e | e <- envs, envFunctionPrefix `isPrefixOf` (fst e)]
envGenFunction (fname, stmt) = genFunction (drop (length envFunctionPrefix) fname) stmt
envUnsetFunctions envs = ["unset " ++ (fst e) | e <- envs, envFunctionPrefix `isPrefixOf` (fst e)]
envFunctionPrefix = "HWK_FUNCTION_"
version = putStrLn "hwk - Haskell based AWK replacement v0.1"
usage   = putStr $ unlines [ "Usage:"
                           , "  hwk env <fname> <stmt>"
                           , "  hwk clear"
                           , "  hwk <stmt>"
                           , ""
                           , "Options:"
                           , "  -h   Show help screen."
                           , "  -v   Show version."
                           ]
