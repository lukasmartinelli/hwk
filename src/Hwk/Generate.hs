module Hwk.Generate where

import Language.Haskell.Exts.Parser (parseStmt, ParseResult, fromParseResult, parseExp, parseModule)

genMain fname = "main = getContents >>= \\contents -> " ++
                "mapM_ putStrLn $ " ++ fname ++ " $ lines contents"

genFunction fname stmt = fname ++ " = " ++ stmt
genModule functions stmt = unlines $ functions ++ [genFunction "hwk" stmt, genMain "hwk"]

--type HwkExpression = String
--type Environment = [(String, String)]

--parse :: HwkExpression -> Environment -> ParseResult HsModule
--parseHwk exp envs = ast
--    where ast = parseExp exp
