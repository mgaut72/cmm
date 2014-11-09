import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import qualified Data.Map as M
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

isGood a = case readD a of
             (Right _, []) -> True
             otherwise     -> False

bad a = TestCase (when (isGood a) (assertFailure ("expected bad parse\ngot: " ++ show (readD a))))

good a = TestCase (unless (isGood a) (assertFailure ("expected good parse\ngot: " ++ show (readD a))))

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b

ls = localSymbols . ix "" . _2

tests = test
  [ "1" ~: FunctionDef TInt "main" (Parameters [ScalarParam TInt "argc", ArrayParam TChar "argv"])
                     []
                     [Return (Just (LitInt 1))]
       |~?= ( (globalSymbols %~ M.insert "main" TInt)
            . (ls %~ M.insert "argc" TInt) . (ls %~ M.insert "argv" (TArray TChar Nothing))
            . (functions %~ M.insert "main" [TInt, TArray TChar Nothing])
            $ tbl )

  , "2" ~: FunctionDef TInt "main" VoidParameter
                     []
                     [Return (Just (LitInt 1))]
       |~?= ( (globalSymbols %~ M.insert "main" TInt)
            . (functions %~ M.insert "main" [])
            $ tbl )

  , "3" ~: FunctionDef TInt "main" (Parameters [ScalarParam TInt "argc", ArrayParam TChar "argv"])
                     []
                     [Return (Just (LitInt 1))]
       |~?= ( (globalSymbols %~ M.insert "main" TInt )
            . (functions %~ M.insert "main" [TInt, TArray TChar Nothing])
            . (ls %~ M.insert "argc" TInt)
            . (ls %~ M.insert "argv" (TArray TChar Nothing))
            $ tbl )

  , "4" ~: FunctionDef TInt "main" (Parameters [ScalarParam TInt "argc", ArrayParam TChar "argv"])
                     []
                     [Return (Just (LitInt 1))]
       |~?= ( (globalSymbols %~ M.insert "main" TInt )
            . (functions %~ M.insert "main" [TInt, TArray TChar Nothing])
            . (ls %~ M.insert "argc" TInt)
            . (ls %~ M.insert "argv" (TArray TChar Nothing))
            $ tbl )

  , "5" ~: FunctionDef TVoid "main" VoidParameter [] []
       |~?= ( (globalSymbols %~ M.insert "main" TVoid)
            . (functions %~ M.insert "main" [])
            $ tbl )

  -- can't have the same parameter identifier in a function
  , "doubleParam" ~: bad $ FunctionDef TVoid "main" (Parameters [ScalarParam TInt "a", ScalarParam TInt "a"]) []
                              [ Return Nothing ]

  -- cant return expression from a void function
  , "voidFunctionReturn" ~: bad $ FunctionDef TVoid "main" VoidParameter []
                              [ Return (Just (LitInt 1)) ]
  , "voidFunctionReturn" ~: good $ FunctionDef TVoid "main" VoidParameter []
                              [ Return Nothing ]
  , "functionReturn" ~: bad $ FunctionDef TInt "main" VoidParameter []
                              [ Return Nothing ]
  , "functionReturn" ~: good $ FunctionDef TInt "main" VoidParameter []
                               [ IfElse (Relative Eq (LitInt 1) (LitInt 2))
                                        (Return (Just (LitInt 1)))
                                        None
                               ]
  ]
