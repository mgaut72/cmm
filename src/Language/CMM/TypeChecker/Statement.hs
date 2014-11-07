module Language.CMM.TypeChecker.Statement where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Lens
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Foldable as F


import Language.CMM.TypeChecker.Expression
import Language.CMM.TypeChecker.Function
import Language.CMM.TypeChecker.Assignment
import Language.CMM.AST
import Language.CMM.Error

typeCheckStatement :: Statement -> MyParser Statement
typeCheckStatement = tcs

tcs ErrorS = return ErrorS

-- we don't need to recursively call tcs since this happens during the parse
tcs x@(If e _) = compatibleWith e TBool >> return x

tcs x@(IfElse e _ _) = compatibleWith e TBool >> return x

tcs x@(While e _) = compatibleWith e TBool >> return x

tcs x@(For ma1 me ma2 _) = checkA ma1 >> checkE me >> checkA ma2 >> return x
 where checkA a = F.forM_ a (void . typeCheckAssignment)
       checkE e = F.forM_ e (void . typeCheckExpression)

tcs x@(Return Nothing) = do
  (expectedT,currF) <- getExpectedType
  unless (expectedT == TVoid) $ err currF
  return x
 where err i = recordError $ "function '" ++ i ++
                             "' is non-void but has a void return statement"

tcs x@(Return (Just e)) = do
  (expectedT,currF) <- getExpectedType
  t <- typeOf e
  unless (expectedT == t) $ err currF expectedT t
  return x
 where err i t1 t2 = recordError $ "function '" ++ i ++ "' has type '" ++
                                   show t1 ++
                                   "' but return statement has type '" ++
                                   show t2 ++ "'"

tcs x@(ProcedureCall f) = typeOfFunction False f
                      >>= (\a -> unless (a == TVoid) err)
                       >> return x
 where err = recordError $ "Cannot call non-void function '" ++
                           i ++ "' in a statement context"
       i = case f of Function ident _ -> ident

tcs None = return None

tcs x@(Assign a) = typeCheckAssignment a >> return x

tcs x@(Bracketed _) = return x

getExpectedType = do
  s <- getState
  let currF = s ^. currFunction
  let (expectedT,_) = fromJust $ M.lookup currF (s ^. localSymbols)
  return (expectedT,currF)
