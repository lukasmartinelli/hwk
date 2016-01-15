module Hwk.Parse where

import Language.Haskell.Exts.Parser (parseStmt, ParseResult, fromParseResult, parseExp, parseModule)
--import Language.Haskell.Interpreter (typeOf)
import Language.Haskell.Interpreter


type HwkExpression = String
type Environment = [(String, String)]

--parse :: HwkExpression -> Environment -> ParseResult HsModule
--parseHwk exp envs = ast
--    where ast = parseExp exp
