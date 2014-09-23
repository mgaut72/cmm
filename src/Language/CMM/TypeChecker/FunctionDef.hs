module Language.CMM.TypeChecker.FunctionDef where

import Control.Lens
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Parsec.Prim

import Language.CMM.AST
import Language.CMM.Error
import Language.CMM.TypeChecker.Declaration

typeCheckFunctionDef f@(FunctionDef t i p vs ss) = checkSignature t i p
                                                >> checkVarDecls vs
                                                >> return f


checkVarDecls = mapM_ (typeCheckDeclaration False . VariableDecl)

checkSignature t i p = do
  s <- getState
  let sTable = view globalSymbols s
  if isNothing (M.lookup i sTable)
    then addPrototype t i p
    else checkExisting t i p

addPrototype t i p = addFcnIdentifier t fs >> addFcnPrototype fs
 where fs = FuncStub i p

checkExisting t i p = checkReturnType t i >> checkParameters i p

checkReturnType t i = do
  s <- getState
  let sTable = view globalSymbols s
  unless (Just t == M.lookup i sTable) err
 where err = recordError $ "Function '" ++ i ++ "' declaration has different "
                        ++ "return type than the declared prototype"

checkParameters i ps = do
  s <- getState
  let fTable = view functions s
  case M.lookup i fTable of
    Nothing              -> err1
    Just expectedTs
      | ts == expectedTs -> return ()
      | otherwise        -> err2
 where err1 = recordError $ "'" ++ i ++ "' was previously declared as a global"
                         ++ " variable but is now being used as a function"
       err2 = recordError $ "Function '" ++ i ++ "' declaration has different "
                         ++ "parameter types than the declared prototype"
       getT (ArrayParam t _) = t
       getT (ScalarParam t _) = t
       ts = case ps of
              Parameters ps -> map getT ps
              VoidParameter -> []




