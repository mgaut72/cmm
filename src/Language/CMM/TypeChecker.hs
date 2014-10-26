module Language.CMM.TypeChecker
  ( module Language.CMM.TypeChecker.Expression
  , module Language.CMM.TypeChecker.Statement
  , module Language.CMM.TypeChecker.Declaration
  , module Language.CMM.TypeChecker.Assignment
  , module Language.CMM.TypeChecker.FunctionDef
  , typeCheck
  )
where

import Control.Monad

import Language.CMM.AST
import Language.CMM.TypeChecker.Expression
import Language.CMM.TypeChecker.Statement
import Language.CMM.TypeChecker.Declaration
import Language.CMM.TypeChecker.Assignment
import Language.CMM.TypeChecker.FunctionDef

typeCheck :: Bool -> (a -> MyParser a) -> a -> MyParser a
typeCheck b tc a = when b (void $ tc a) >> return a

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
