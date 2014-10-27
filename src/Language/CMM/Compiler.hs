module Language.CMM.Compiler (
  compileCMM
)
where

import Text.Parsec
import qualified Data.Map as M
import Data.List
import Control.Monad.Writer

import Language.CMM.AST
import Language.CMM.Parser.Typed
import Language.CMM.Parser.Base

compileCMM = makeMessages . runWriter . runParserT p initialTables "compile"
 where p = do
         whiteSpace
         prog <- programP
         eof
         s <- getState
         return (prog, s)

makeMessages (a, w) = (a, msgs)
 where msgs = unlines . map outputToMsg . nubBy (\a b -> snd a == snd b) $ w
       outputToMsg (p, m) = "Error near line " ++ show (sourceLine p) ++
                            ", column " ++ show (sourceColumn p) ++
                            ":\n\t" ++ m ++ "\n\n"
