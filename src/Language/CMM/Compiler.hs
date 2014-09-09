module Language.CMM.Compiler (
  compileCMM
)
where

import Text.ParserCombinators.Parsec
import Language.CMM.Syntax.Parser

compileCMM = readExpr

readExpr = parse ep "compile"
 where ep = do
         whiteSpace
         e <- statementP
         eof
         return e
