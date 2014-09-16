module Language.CMM.Compiler (
  compileCMM
)
where

import Text.ParserCombinators.Parsec
import Data.Map.Strict as M

import Language.CMM.AST
import Language.CMM.Parser.UnTyped
import Language.CMM.Parser.Base

compileCMM = readExpr

readExpr = runParser ep initialTables "compile"
 where ep = do
         whiteSpace
         e <- expressionP
         eof
         return e
