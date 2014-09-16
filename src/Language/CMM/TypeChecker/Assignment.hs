module Language.CMM.TypeChecker.Assignment where

import Text.ParserCombinators.Parsec
import Language.CMM.TypeChecker.Expression
import Language.CMM.AST

typeCheckAssignment :: Assignment -> MyParser Assignment
typeCheckAssignment a@(Assignment v e) = do
  tvar <- typeOf $ Var v
  te   <- typeOf e
  case (tvar, te) of
    (Left m, _)           -> unexpected $ "type error: " ++ m
    (_, Left m)           -> unexpected $ "type error: " ++ m
    (Right t1, Right t2)
      | t1 == t2 && t2 == TChar -> return a
      | otherwise         -> unexpected $ "type error: variable and expression have incompatible types in assignment : " ++ show a
