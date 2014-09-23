module Language.CMM.TypeChecker.Declaration where

import Control.Monad
import Control.Lens
import Data.Maybe
import Data.List (nub)
import qualified Data.Map.Strict as M
import Text.Parsec.Prim

import Language.CMM.AST
import Language.CMM.Error

typeCheckDeclaration :: Bool -> Declaration -> MyParser Declaration

typeCheckDeclaration isGlobal d = case d of
  VariableDecl (VarDecl t vs)   -> mapM_ (addVarIdentifier isGlobal t) vs
                                >> return d
  FunctionDecl isExtern t stubs -> mapM_ (addFcnIdentifier t) stubs
                                >> mapM_ addFcnPrototype stubs
                                >> return d

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
       getT (ArrayParam t _) = t
       getT (ScalarParam t _) = t

addVarIdentifier :: Bool -> TType -> Variable -> MyParser ()
addVarIdentifier isGlobal t v = checkVariable isGlobal v
                             >> modifyState (table %~ M.insert i t)
 where i = getI v
       table = if isGlobal then globalSymbols else localSymbols
       getI (Array i (LitInt s)) = i
       getI (Scalar i) = i

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
  let sTable = if isGlobal then view globalSymbols s else view localSymbols s
  unless (isNothing $ M.lookup i sTable)
         (recordError $ "Identifier '" ++ i ++ "' is already identified in the current scope")
