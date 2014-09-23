module Langauge.CMM.TypeChecker.FunctionDef where

import Language.CMM.AST
import Language.CMM.Error
import Lanugauge.CMM.TypeChecker.Declaration

typeCheckFunctionDef f@(t i p vs ss) = checkSignature t i p

checkSignature t i p = do
  s <- getState
  let sTable = view globalSymbols s
  if isNothing (m.lookup i sTable)
    then addPrototype t i p
    else checkExisting t i p

addPrototype t i p = addFcnIdentifier t fs >> addFcnPrototype fs
 where fs = FuncStud i p

checkExisting t i p = checkReturnType t i >> checkParamers i p

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
       getI (ArrayParam _ i) = i
       getI (ScalarParam _ i) = i
       ts = map getI ps




