module Language.CMM.Compiler (
  compileCMM
)
where

import Text.ParserCombinators.Parsec
import Data.Map.Strict as M

import Language.CMM.Syntax.AST
import Language.CMM.Syntax.Parser.UnTyped
import Language.CMM.Syntax.Parser.Base
import Language.CMM.Syntax.TypeChecker

compileCMM = readExpr

initialTables = Tables { _symbols = M.empty
                       , _functions = M.empty
                       }

readExpr = runParser ep initialTables "compile"
 where ep = do
         whiteSpace
         e <- expressionP
         eof
         return e
