module Language.CMM.TypeChecker.Assignment where

import Text.ParserCombinators.Parsec
import Control.Monad

import Language.CMM.TypeChecker.Expression
import Language.CMM.AST

typeCheckAssignment :: Assignment -> MyParser Assignment
typeCheckAssignment a@(Assignment v e) = do
  tvar <- typeOf $ Var v
  te   <- typeOf e
  unless (tvar == TChar) (unexpected $ "cannot assign to type '" ++ show tvar ++ "'")
  if tvar == te
    then return a
    else unexpected $ "type error: variable and expression have incompatible types in assignment : " ++ show a
