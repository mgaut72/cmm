module Language.CMM.TypeChecker.Assignment where

import Text.ParserCombinators.Parsec
import Control.Monad

import Language.CMM.AST
import Language.CMM.Error
import Language.CMM.TypeChecker.Expression

typeCheckAssignment :: Assignment -> MyParser Assignment
typeCheckAssignment a@(Assignment v e) = do
  tvar <- typeOf $ Var v
  te   <- typeOf e
  unless (tvar `compatible` TChar) (recordError $ "cannot assign to type '" ++ show tvar ++ "'")
  unless (tvar `compatible` te) (recordError $ "type error: variable type '"
                                 ++ show tvar ++ "' and expression type '"
                                 ++ show te ++ "' have incompatible types in assignment : "
                                 ++ show a)
  return a
