module Language.CMM.TypeChecker.Function where

import Control.Monad
import Control.Lens

import qualified Data.Map as M

import Text.Parsec

import Language.CMM.AST
import Language.CMM.Error
import Language.CMM.TypeChecker.Expression

typeOfFunction :: Bool -> Function -> MyParser TType
typeOfFunction isExpression (Function i es) = do
  retType <- lookUpFunctionType i
  case retType of
    TError -> return TError
    _      -> do expectedTs <- lookUpArgs i
                 case expectedTs of
                   [TError] -> return TError
                   _        -> do actualTs <- mapM typeOf es
                                  unless (allEqual expectedTs actualTs) err1
                                  when (isExpression && retType == TVoid) err2
                                  return retType
 where allEqual t t' = length t == length t' && and (zipWith compatible t t')
       err1 = recordError $ "type error : Function arguments for function '" ++
                            i ++ "' have incorrect type"
       err2 = recordError $ "Cannot call void function '" ++
                            i ++ "' from an expression context"

lookUpFunctionType i = do
  s <- getState
  let tglo = M.lookup i $ s ^. globalSymbols
  case tglo of
      Just t -> return t
      _      -> err m1
 where m1 = "Identifier '" ++ i ++ "' not found in scope"
