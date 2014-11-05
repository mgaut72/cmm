module Language.CMM.Error where

import Control.Monad.Writer
import Text.Parsec

import Language.CMM.AST

recordError :: String -> MyParser ()
recordError m = do
      p <- getPosition
      lift $ tell [(p,m)]
