module Language.CMM.TypeChecker.FunctionDef where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Text.Parsec.Prim

import Language.CMM.AST
import Language.CMM.Error
import Language.CMM.TypeChecker.Declaration

-- we rely on the declarations and statements having already been
-- typechecked when they were parsed
typeCheckFunctionDef :: FunctionDef -> MyParser FunctionDef
typeCheckFunctionDef f@(FunctionDef t i p _ ss) = checkSignature t i p
                                               >> addParameters p
                                               >> checkReturns i t ss
                                               >> return f


checkSignature :: TType -> Identifier -> Parameters -> MyParser ()
checkSignature t i p = do
  s <- getState
  let sTable = view globalSymbols s
  let eSet   = view externFunctions s
  when (S.member i eSet) err1
  if isNothing (M.lookup i sTable)
    then addPrototype t i p
    else checkExisting t i p
  addParameters p
 where err1 = recordError $ "'" ++ i ++
                            "' is declared to be an extern function, " ++
                            "but has a corresponding definition"

addPrototype :: TType -> Identifier -> Parameters -> MyParser ()
addPrototype t i p = addFcnIdentifier t fs >> addFcnPrototype fs
 where fs = FuncStub i p

checkExisting :: TType -> Identifier -> Parameters -> MyParser ()
checkExisting t i p = checkReturnType t i >> checkParameters i p

checkReturnType :: TType -> Identifier -> MyParser ()
checkReturnType t i = do
  s <- getState
  let sTable = view globalSymbols s
  unless (Just t == M.lookup i sTable) err
 where err = recordError $ "Function '" ++ i ++
                           "' declaration has different " ++
                           "return type than the declared prototype"

checkReturns :: Identifier -> TType -> [Statement] -> MyParser ()
checkReturns i t ss
  | t == TVoid = unless voidCase errVoid
  | otherwise  = unless exprCase1 errE1 >> unless exprCase2 errE2
 where voidCase = not hasReturnExpr
       exprCase1 = hasReturnExpr
       exprCase2 = not hasReturnNothing
       hasReturnExpr = or $ mapStatement isReturnExpr ss
       hasReturnNothing = or $ mapStatement isReturnNone ss
       isReturnExpr s = case s of
         Return (Just _) -> True
         _               -> False
       isReturnNone s = case s of
         Return Nothing -> True
         _              -> False
       errVoid = recordError $ "Function '" ++ i ++
                               "' is declared void but returns an expression"
       errE1 = recordError $ "Function '" ++ i ++
                             "' is non-void, but does not return an expression"
       errE2 = recordError $ "Function '" ++ i ++
                             "' is non-void, but has a statement 'return;'"

checkParameters :: Identifier -> Parameters -> MyParser ()
checkParameters i ps = do
  s <- getState
  let fTable = view functions s
  case M.lookup i fTable of
    Nothing              -> err1
    Just expectedTs
      | ts == expectedTs -> return ()
      | otherwise        -> err2
 where err1 = recordError $ "'" ++ i ++
                            "' was previously declared as a global" ++
                            " variable but is now being used as a function"
       err2 = recordError $ "Function '" ++ i ++
                            "' declaration has different " ++
                            "parameter types than the declared prototype"
       getT (ArrayParam t _) = TArray t Nothing
       getT (ScalarParam t _) = t
       ts = case ps of
              Parameters ps' -> map getT ps'
              VoidParameter  -> []

addParameters :: Parameters -> MyParser ()
addParameters VoidParameter = return ()
addParameters (Parameters ps) = mapM_ addP ps
 where addP p = do
         table <- lTable
         modifyState (table %~ M.insert (getI p) (getT p))
       getI (ArrayParam _ i) = i
       getI (ScalarParam _ i) = i
       getT (ArrayParam t _) = TArray t Nothing
       getT (ScalarParam t _) = t
