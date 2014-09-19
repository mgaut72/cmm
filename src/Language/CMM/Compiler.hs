module Language.CMM.Compiler (
  compileCMM
)
where

import Text.Parsec
import Data.Map.Strict as M

import Language.CMM.AST
import Language.CMM.Parser.UnTyped
import Language.CMM.Parser.Base

compileCMM = runParserT p initialTables "compile"
 where p = do
         whiteSpace
         prog <- programP
         eof
         return prog
