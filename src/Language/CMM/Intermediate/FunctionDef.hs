module Language.CMM.Intermediate.FunctionDef where

import Control.Applicative
import Control.Monad
import Data.Monoid

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions
import Language.CMM.Intermediate.Statement

genF :: FunctionDef -> TACGen [ThreeAddress]
genF (FunctionDef _ i _ _ ss) = do
  body <- liftM concat . mapM genS $ ss
  return $ pure (Label i) <> pure (Enter i) <> body <> pure (Leave i)
