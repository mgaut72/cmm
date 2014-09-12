module Language.CMM.Compiler (
  compileCMM
)
where

import Text.ParserCombinators.Parsec
import Data.Map.Strict as M

import Language.CMM.Syntax.Parser
import Language.CMM.Syntax.TypeChecker

compileCMM = readExpr

initialTables = Tables { _symbols = M.empty
                       , _functions = M.empty
                       }

readExpr = runParser ep initialTables "compile"
 where ep = do
         whiteSpace
         e <- statementP
         eof
         return e
