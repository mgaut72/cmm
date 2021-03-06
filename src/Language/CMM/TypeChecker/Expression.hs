module Language.CMM.TypeChecker.Expression where

import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Text.Parsec.Prim

import Language.CMM.AST
import Language.CMM.Error
import {-# SOURCE #-} Language.CMM.TypeChecker.Function

typeCheckExpression :: Expression -> MyParser Expression
typeCheckExpression e = typeOf e >> return e

compatibleWith :: Expression -> TType -> MyParser TType
compatibleWith e t = do
  et <- typeOf e
  if t `compatible` et
    then return t
    else err $ "type error: '" ++ show e ++
               "' is not compatible with type '" ++ show t ++ "'"

allCompatibleWith :: [Expression] -> TType -> MyParser TType
allCompatibleWith es t = mapM_ (`compatibleWith` t) es >> return t

typeOf :: Expression -> MyParser TType

typeOf ErrorE = return TError

typeOf (LitInt _) = return TInt

typeOf (LitChar _) = return TChar

typeOf (LitString _) = return (TArray TChar Nothing)

typeOf (Negative e) = compatibleWith e TInt >> return TInt

typeOf (Not e) = compatibleWith e TBool >> return TBool

typeOf (Binary _ e1 e2) = allCompatibleWith [e1,e2] TInt >> return TInt

typeOf (Relative _ e1 e2) = allCompatibleWith [e1,e2] TInt >> return TBool

typeOf (Logical _ e1 e2) = allCompatibleWith [e1,e2] TBool >> return TBool

typeOf (FunctionCall f) = typeOfFunction True f

typeOf (Var (Scalar i)) = lookUpSymb i

typeOf (Var (Array i e)) = do
  compatibleWith e TInt
  t <- lookUpSymb i
  case t of
    (TArray t' _) -> return t'
    _ -> recordError "Array index performed on non-array variable" >> return t


err :: String -> MyParser TType
err m = recordError m >> return TError

lookUpArgs :: Identifier -> MyParser [TType]
lookUpArgs i = do
  s <- getState
  case M.lookup i (view functions s) of
    Just ts -> return ts
    Nothing -> liftM (:[]) $ err $ "Identifier '" ++ i ++
                                   "' is not a function found in scope"

lookUpSymb :: Identifier -> MyParser TType
lookUpSymb i = do
  s <- getState
  let currF = s ^. currFunction
  let locTab = s ^. localSymbols . ix currF . _2
  let tloc = M.lookup i locTab
  let tglo = M.lookup i $ s ^. globalSymbols
  case (tloc, tglo) of
      (Just t, _) -> return t
      (_, Just t)
        | M.member i (s ^. functions) -> err m1
        | otherwise                   -> return t
      _ -> err m2
 where m1 = "Identifier '" ++ i ++ "' is a function, not a variable"
       m2 = "Identifier '" ++ i ++ "' not found in scope"
