{-# LANGUAGE TemplateHaskell #-}
module Language.CMM.Syntax.TypeChecker where

import Data.Map.Strict as M
import Control.Monad.State
import Control.Lens

import Language.CMM.Syntax.AST

data TType = TBool
           | TChar
           | TInt
           | TVoid
           | TArray TType
           deriving (Show)

instance Eq TType where
  TBool == TBool = True
  TChar == TChar = True
  TInt  == TInt  = True
  TVoid == TVoid = True
  TChar == TInt  = True
  TInt  == TChar = True
  TArray t1 == TArray t2 = t1 == t2
  (==) _ _ = False


type SymbolTable = M.Map Identifier TType
type FunctionArgumentTable = M.Map Identifier [TType]

data Tables = Tables { _symbols :: SymbolTable
                     , _functions :: FunctionArgumentTable
                     }

makeLenses ''Tables

type TypeChecker a = State Tables (Either String a)

lookUpArgs :: Identifier -> TypeChecker [TType]
lookUpArgs i = do
  fcns <- use functions
  let t = M.lookup i fcns
  case t of
    Just t  -> return $ Right t
    Nothing -> return $ Left ("Identifier '" ++ i ++ "' is not a function found in scope")


lookUpSymb :: Identifier -> TypeChecker TType
lookUpSymb i = do
  symbs <- use symbols
  let t = M.lookup i symbs
  case t of
      Just t  -> return $ Right t
      Nothing -> return $ Left ("Identifier " ++ i ++ " not found in scope")

compatibleWith :: Expression -> TType -> TypeChecker TType
compatibleWith e t = do
  et <- typeOf e
  case et of
    Right t1 | t1 == t   -> return $ Right t
             | otherwise -> return $ Left (show e ++ " is not compatible with " ++ show t)
    m -> return m

allCompatibleWith :: [Expression] -> TType -> TypeChecker TType
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



typeOf :: Expression -> TypeChecker TType

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
    otherwise -> lookUpSymb i
