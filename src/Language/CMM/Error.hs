module Language.CMM.Error where

import Control.Monad.Writer
import Text.Parsec
import Data.List

import Language.CMM.AST

recordError :: String -> MyParser ()
recordError m = do
      p <- getPosition
      lift $ tell [(p,m)]
