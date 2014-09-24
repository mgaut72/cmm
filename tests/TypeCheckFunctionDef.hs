import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Error

import Language.CMM.AST
import Language.CMM.TypeChecker.FunctionDef

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

readD ds = runWriter . runParserT p tbl "" $ ""
 where p = typeCheckFunctionDef ds >> getState

tbl = initialTables

as |~?= b = readD as ~?= (Right b, [])

bad a = TestCase (when (null errs) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where (res, errs) = readD a
       isLeft (Right _) = False
       isLeft (Left _) = True

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b


tests = test
  [ "1" ~: FunctionDef TInt "main" (Parameters [ScalarParam TInt "argc", ArrayParam TChar "argv"])
                     []
                     []
       |~?= ( (globalSymbols %~ M.insert "main" TInt)
            . (localSymbols %~ M.insert "argc" TInt)
            . (localSymbols %~ M.insert "argv" (TArray TChar))
            . (functions %~ M.insert "main" [TInt, TArray TChar])
            $ tbl )

  , "2" ~: FunctionDef TInt "main" VoidParameter
                     []
                     [ None ]
       |~?= ( (globalSymbols %~ M.insert "main" TInt)
            . (functions %~ M.insert "main" [])
            $ tbl )

  , "3" ~: FunctionDef TInt "main" (Parameters [ScalarParam TInt "argc", ArrayParam TChar "argv"])
                     []
                     []
       |~?= ( (globalSymbols %~ M.insert "main" TInt )
            . (functions %~ M.insert "main" [TInt, TArray TChar])
            . (localSymbols %~ M.insert "argc" TInt)
            . (localSymbols %~ M.insert "argv" (TArray TChar))
            $ tbl )

  , "4" ~: FunctionDef TInt "main" (Parameters [ScalarParam TInt "argc", ArrayParam TChar "argv"])
                     []
                     []
       |~?= ( (globalSymbols %~ M.insert "main" TInt )
            . (functions %~ M.insert "main" [TInt, TArray TChar])
            . (localSymbols %~ M.insert "argc" TInt)
            . (localSymbols %~ M.insert "argv" (TArray TChar))
            $ tbl )

  , "5" ~: FunctionDef TVoid "main" VoidParameter [] []
       |~?= ( (globalSymbols %~ M.insert "main" TVoid)
            . (functions %~ M.insert "main" [])
            $ tbl )
  ]
