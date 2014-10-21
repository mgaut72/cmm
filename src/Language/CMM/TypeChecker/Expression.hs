module Language.CMM.TypeChecker.Expression where

import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

import Language.CMM.AST
import Language.CMM.Error

typeCheckExpression :: Expression -> MyParser Expression
typeCheckExpression e = typeOf e >> return e

err m = recordError m >> return TError

lookUpArgs :: Identifier -> MyParser [TType]
lookUpArgs i = do
  s <- getState
  case M.lookup i (view functions s) of
    Just ts -> return ts
    Nothing -> liftM (:[]) $ err $ "Identifier '" ++ i ++ "' is not a function found in scope"


lookUpSymb :: Identifier -> MyParser TType
lookUpSymb i = do
  s <- getState
  let tloc = M.lookup i (view localSymbols s)
  let tglo = M.lookup i (view globalSymbols s)
  case (tloc, tglo) of
      (Just t, _)  -> return t
      (_, Just t)  -> return t
      otherwise    -> err $ "Identifier " ++ i ++ " not found in scope"

compatibleWith :: Expression -> TType -> MyParser TType
compatibleWith e t = do
  et <- typeOf e
  if t `compatible` et
    then return t
    else err $ "type error: '" ++ show e ++ "' is not compatible with type '" ++ show t ++ "'"

allCompatibleWith :: [Expression] -> TType -> MyParser TType
allCompatibleWith es t = mapM_ (`compatibleWith` t) es >> return t

typeOf :: Expression -> MyParser TType

typeOf (LitInt _) = return TInt

typeOf (LitChar _) = return TChar

typeOf (LitString _) = return (TArray TChar)

typeOf (Negative e) = compatibleWith e TInt >> return TInt

typeOf (Not e) = compatibleWith e TBool >> return TBool

typeOf (Binary _ e1 e2) = allCompatibleWith [e1,e2] TInt >> return TInt

typeOf (Relative _ e1 e2) = allCompatibleWith [e1,e2] TInt >> return TBool

typeOf (Logical _ e1 e2) = allCompatibleWith [e1,e2] TBool >> return TBool

typeOf (FunctionCall (Function i es)) = do
  expectedTypes <- lookUpArgs i
  actualTypes <- mapM typeOf es
  unless (allEqual expectedTypes actualTypes) $ void $ err $ "type error : Function arguments for function '" ++ i ++ "' have incorrect type"
  retType <- lookUpSymb i
  when (retType == TVoid) $ void $ err $ "Cannot call void function '" ++ i ++ "' from an expression context"
  return retType
 where allEqual t1 t2 = length t1 == length t2 && and (zipWith compatible t1 t2)

typeOf (Var (Scalar i)) = lookUpSymb i

typeOf (Var (Array i e)) = do
  compatibleWith e TInt
  t <- lookUpSymb i
  case t of
    (TArray at) -> return at
    otherwise   -> unexpected "Array index performed on non-array variable"
