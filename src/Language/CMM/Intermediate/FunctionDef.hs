module Language.CMM.Intermediate.FunctionDef where

import Control.Applicative
import Data.Foldable (foldMap)
import Data.Monoid

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions

genF :: FunctionDef -> TACGen [ThreeAddress]
genF (FunctionDef t i ps vs ss) = do
  body <- foldMap genS ss
  return $ pure (Call i) <> body
