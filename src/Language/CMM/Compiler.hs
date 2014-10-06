module Language.CMM.Compiler (
  compileCMM
)
where

import Text.Parsec
import qualified Data.Map as M
import Control.Monad.Writer

import Language.CMM.AST
import Language.CMM.Parser.UnTyped
import Language.CMM.Parser.Base

compileCMM inp = runWriter (runParserT p initialTables "compile" inp)
 where p = do
         whiteSpace
         prog <- programP
         eof
         return prog
