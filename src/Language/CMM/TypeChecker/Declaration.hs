module Language.CMM.TypeChecker.Declaration where

import Control.Monad
import Control.Lens
import Data.Maybe
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec.Prim

import Language.CMM.AST
import Language.CMM.Error

typeCheckDeclaration :: Bool -> Declaration -> MyParser Declaration
typeCheckDeclaration isGlobal d = case d of
  VariableDecl (VarDecl t vs)   -> mapM_ (addVarIdentifier isGlobal t) vs
                                >> return d
  FunctionDecl isExtern t stubs -> mapM_ (addFcnIdentifier t) stubs
                                >> mapM_ addFcnPrototype stubs
                                >> when isExtern (mapM_ addExtern stubs)
                                >> return d

addExtern :: FuncStub -> MyParser ()
addExtern (FuncStub ident _) = modifyState $ externFunctions %~ S.insert ident

addFcnIdentifier :: TType -> FuncStub -> MyParser ()
addFcnIdentifier t f = checkStub f
                    >> modifyState (globalSymbols %~ M.insert i t)
 where i = getI f
       getI (FuncStub ident _) = ident

addFcnPrototype :: FuncStub -> MyParser ()
addFcnPrototype (FuncStub i p) = modifyState $ functions %~ M.insert i pTypes
 where pTypes = case p of
                  VoidParameter -> []
                  Parameters ps -> map getT ps
       getT (ArrayParam t _) = TArray t
       getT (ScalarParam t _) = t

addVarIdentifier :: Bool -> TType -> Variable -> MyParser ()
addVarIdentifier isGlobal t v = do
  checkVariable isGlobal v
  table <- if isGlobal then gTable else lTable
  modifyState (table %~ M.insert i (getT v t))
 where i = getI v
       getI (Array i _) = i
       getI (Scalar i) = i
       getT (Array _ _) = TArray
       getT (Scalar _) = id

checkStub (FuncStub i params) = checkIdentifier True i >> checkParams params

checkParams VoidParameter = return ()
checkParams (Parameters ps) = unless (nub is == is) idError
 where is = map getI ps
       idError = recordError $ "repeated identifier in function parameters '" ++ show ps ++ "'"
       getI (ArrayParam _ i) = i
       getI (ScalarParam _ i) = i


checkVariable isGlobal (Array i (LitInt s)) = validateSize >> validateId
 where validateId = checkIdentifier isGlobal i
       validateSize = unless (s > 0) $
                      recordError "declaration : array index must be greater than 0"

checkVariable isGlobal (Scalar i) = checkIdentifier isGlobal i

checkIdentifier isGlobal i = do
  s <- getState
  table <- if isGlobal then gTable else lTable
  unless (isNothing $ M.lookup i $ s ^. table)
         (recordError $ "Identifier '" ++ i ++ "' is already identified in the current scope")
