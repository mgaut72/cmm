module Language.CMM.Compiler (
  parseCMM
)
where

import Text.ParserCombinators.Parsec
import Language.CMM.Syntax.Parser

compileCMM = readExpr

readExpr s = parse ep "compile" s
 where ep = do
         whiteSpace
         e <- expressionP
         whiteSpace
         eof
         return e
