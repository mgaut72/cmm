module Language.CMM.Error where

import Control.Monad.Writer
import Text.Parsec

import Language.CMM.AST

recordError :: String -> MyParser ()
recordError m = do
  p <- getPosition
  let l = sourceLine p
  let c = sourceColumn p
  let msg = "Error near line " ++ show l ++ ", column " ++ show c ++ ":\n\t" ++ m ++ "\n\n"
  lift $ tell [msg]


