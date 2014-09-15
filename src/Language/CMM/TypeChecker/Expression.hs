module Language.CMM.TypeChecker.Expression where

import Data.Map.Strict as M
import Control.Monad.State
import Control.Lens
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

import Language.CMM.AST

typeCheckExpression :: Expression -> MyParser Expression
typeCheckExpression e = do
  t <- typeOf e
  case t of
    Right t -> return e
    Left m -> unexpected $ "type error: " ++ m


lookUpArgs :: Identifier -> MyParser (Either String [TType])
lookUpArgs i = do
  s <- getState
  let t = M.lookup i (view functions s)
  case t of
    Just t  -> return $ Right t
    Nothing -> return $ Left ("Identifier '" ++ i ++ "' is not a function found in scope")


lookUpSymb :: Identifier -> MyParser (Either String TType)
lookUpSymb i = do
  s <- getState
  let t = M.lookup i (view symbols s)
  case t of
      Just t  -> return $ Right t
      Nothing -> return $ Left ("Identifier " ++ i ++ " not found in scope")

compatibleWith :: Expression -> TType -> MyParser (Either String TType)
compatibleWith e t = do
  et <- typeOf e
  case et of
    Right t1 | t1 == t   -> return $ Right t
             | otherwise -> return $ Left (show e ++ " is not compatible with " ++ show t)
    m -> return m

allCompatibleWith :: [Expression] -> TType -> MyParser (Either String TType)
allCompatibleWith [e] t = compatibleWith e t
allCompatibleWith (e:es) t = do
  t1 <- compatibleWith e t
  t2 <- allCompatibleWith es t
  case (t1,t2) of
    (Left m, Left m1)    -> return $ Left (unlines [m,m1])
    (Left m, _)          -> return $ Left m
    (_ , Left m)         -> return $ Left m
    (Right ta, Right tb)
      | ta == tb         -> return $ Right t
      | otherwise        -> return $ Left ("Types '" ++ show ta ++ "' and '" ++ show tb ++ "' dont match")



typeOf :: Expression -> MyParser (Either String TType)

typeOf (LitInt _) = return $ Right TInt

typeOf (LitChar _) = return $ Right TChar

typeOf (LitString _) = return $ Right (TArray TChar)

typeOf (Negative e) = compatibleWith e TInt

typeOf (Not e) = compatibleWith e TBool

typeOf (Binary _ e1 e2) = allCompatibleWith [e1,e2] TInt

typeOf (Relative _ e1 e2) = do
  a <- allCompatibleWith [e1,e2] TInt
  case a of
    Right TInt -> return $ Right TBool
    Left  m    -> return $ Left m

typeOf (Logical _ e1 e2) = allCompatibleWith [e1,e2] TBool

typeOf (FunctionCall (Function i es)) = do
  argTypes <- lookUpArgs i
  actualTypes <- mapM typeOf es
  let ats = sequence actualTypes
  case (argTypes, ats) of
    (Left m, _)   -> return $ Left m
    (_, Left m)   -> return $ Left m
    (Right t1, Right t2)
      | t1 == t2  -> lookUpSymb i
      | otherwise -> return $ Left "Function arguments have incorrect type"

typeOf (Var (Scalar i)) = lookUpSymb i

typeOf (Var (Array i e)) = do
  et <- compatibleWith e TInt
  case et of
    Left m    -> return $ Left m
    otherwise -> do
      t <- lookUpSymb i
      case t of
        Right (TArray at) -> return $ Right at
        otherwise        -> return $ Left "Array index performed on non-array variable"
