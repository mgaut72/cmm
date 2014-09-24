module Language.CMM.TypeChecker.Statement where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Lens

import Language.CMM.TypeChecker.Expression
import Language.CMM.TypeChecker.Assignment
import Language.CMM.AST
import Language.CMM.Error

typeCheckStatement :: Statement -> MyParser Statement

typeCheckStatement x@(If e s) = compatibleWith e TBool
                             >> return x

typeCheckStatement x@(IfElse e s1 s2) = compatibleWith e TBool
                                     >> return x

typeCheckStatement x@(While e s) = compatibleWith e TBool
                                >> return x

typeCheckStatement x@(For ma1 me ma2 s) = do
  case ma1 of
    Just a1 -> void $ typeCheckAssignment a1
    _       -> return ()
  case me of
    Just e -> void $ typeCheckExpression e
    _      -> return ()
  case ma2 of
    Just a2 -> void $ typeCheckAssignment a2
    _       -> return ()
  return x


typeCheckStatement x@(Return Nothing) = do
  s <- getState
  let expectedT = view currentFunctionType s
  unless (expectedT == TVoid) err
  return x
 where err =  recordError "Type error: Current function is non-void but there is a void return statement"

typeCheckStatement x@(Return (Just e)) = do
  s <- getState
  let expectedT = view currentFunctionType s
  t <- typeOf e
  unless (expectedT == t) $ err expectedT t
  return x
 where err t1 t2 = recordError $ "Type error: Current function has type '"
          ++ show t1 ++ "' but return statement has type '"
          ++ show t2 ++ "'"

typeCheckStatement x@(ProcedureCall f) = typeCheckExpression (FunctionCall f)
                                      >> return x

typeCheckStatement None = return None

typeCheckStatement x@(Assign a) = typeCheckAssignment a >> return x

typeCheckStatement x@(Bracketed ss) = return x
