module Language.CMM.TypeChecker.Expression where

import Data.Map.Strict as M
import Control.Monad.State
import Control.Lens
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

import Language.CMM.AST

typeCheckExpression :: Expression -> MyParser Expression
typeCheckExpression e = typeOf e >> return e


lookUpArgs :: Identifier -> MyParser [TType]
lookUpArgs i = do
  s <- getState
  case M.lookup i (view functions s) of
    Just ts -> return ts
    Nothing -> unexpected $ "Identifier '" ++ i ++ "' is not a function found in scope"


lookUpSymb :: Identifier -> MyParser TType
lookUpSymb i = do
  s <- getState
  let tloc = M.lookup i (view localSymbols s)
  let tglo = M.lookup i (view globalSymbols s)
  case (tloc, tglo) of
      (Just t, _)  -> return t
      (_, Just t)  -> return t
      otherwise    -> unexpected $ "Identifier " ++ i ++ " not found in scope"

compatibleWith :: Expression -> TType -> MyParser TType
compatibleWith e t = do
  et <- typeOf e
  if t == et
    then return t
    else unexpected $ "type error: '" ++ show e ++ "' is not compatible with type '" ++ show t ++ "'"

allCompatibleWith :: [Expression] -> TType -> MyParser TType
allCompatibleWith es t = mapM_ (`compatibleWith` t) es >> return t

typeOf :: Expression -> MyParser TType

typeOf (LitInt _) = return TInt

typeOf (LitChar _) = return TChar

typeOf (LitString _) = return (TArray TChar)

typeOf (Negative e) = compatibleWith e TInt

typeOf (Not e) = compatibleWith e TBool

typeOf (Binary _ e1 e2) = allCompatibleWith [e1,e2] TInt

typeOf (Relative _ e1 e2) = allCompatibleWith [e1,e2] TInt >> return TBool

typeOf (Logical _ e1 e2) = allCompatibleWith [e1,e2] TBool

typeOf (FunctionCall (Function i es)) = do
  expectedTypes <- lookUpArgs i
  actualTypes <- mapM typeOf es
  if expectedTypes == actualTypes
    then lookUpSymb i
    else unexpected "type error : Function arguments have incorrect type"

typeOf (Var (Scalar i)) = lookUpSymb i

typeOf (Var (Array i e)) = do
  compatibleWith e TInt
  t <- lookUpSymb i
  case t of
    (TArray at) -> return at
    otherwise   -> unexpected "Array index performed on non-array variable"
