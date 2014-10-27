module Language.CMM.TypeChecker.Assignment where

import Control.Monad

import Language.CMM.AST
import Language.CMM.Error
import Language.CMM.TypeChecker.Expression

typeCheckAssignment :: Assignment -> MyParser Assignment
typeCheckAssignment ErrorA = return ErrorA
typeCheckAssignment a@(Assignment v e) = do
  tvar <- typeOf $ Var v
  te   <- typeOf e
  unless (tvar `compatible` TChar) $ err1 tvar
  unless (tvar `compatible` te) $ err2 tvar te
  return a
 where err1 t = recordError $ "cannot assign to type '" ++ show t ++ "'"
       err2 t1 t2 = recordError $ "type error: variable type '" ++ show t1 ++
                                  "' and expression type '" ++ show t2 ++
                                  "' have incompatible types in assignment"
