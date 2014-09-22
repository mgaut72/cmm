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
import Language.CMM.TypeChecker.Declaration

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

readD ds = runWriter . runParserT p initialTables "" $ ""
 where p = mapM_ (typeCheckDeclaration False) ds >> getState

as |~?= b = fst (readD as) ~?= Right b

bad a = TestCase (when (null errs) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where (res, errs) = readD a
       isLeft (Right _) = False
       isLeft (Left _) = True

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b


tests = test
  [ "vd" ~: [ VariableDecl (VarDecl TInt [Scalar "i"]) ] |~?=
            ( localSymbols %~ M.insert "i" TInt  $ initialTables )
  , "vd" ~: [ VariableDecl (VarDecl TInt [Scalar "i", Scalar "j"]) ] |~?=
            ( (localSymbols %~ M.insert "i" TInt)
            . (localSymbols %~ M.insert "j" TInt)
            $ initialTables )
  , "vd" ~: [ VariableDecl (VarDecl TInt [Scalar "i"])
            , VariableDecl (VarDecl TInt [Scalar "j"])
            ] |~?=
            ( (localSymbols %~ M.insert "i" TInt)
            . (localSymbols %~ M.insert "j" TInt)
            $ initialTables )
  , "vd" ~: [ VariableDecl (VarDecl TInt [Scalar "i", Scalar "j"])
            , FunctionDecl False TVoid [FuncStub "f" VoidParameter]
            ] |~?=
            ( (localSymbols %~ M.insert "i" TInt)
            . (localSymbols %~ M.insert "j" TInt)
            . (globalSymbols %~ M.insert "f" TVoid)
            . (functions  %~ M.insert "f" [])
            $ initialTables )
  -- Same Var declared twice
  , "vd" ~: bad [ VariableDecl (VarDecl TInt [Scalar "i"])
                , VariableDecl (VarDecl TInt [Scalar "i"])
                ]
  , "vd" ~: bad [ VariableDecl (VarDecl TInt [Scalar "i", Scalar "i"]) ]
  , "vd" ~: bad [ VariableDecl (VarDecl TInt [Scalar "i", Scalar "j"])
                , FunctionDecl False TVoid [ FuncStub "f" VoidParameter
                                           , FuncStub "f" VoidParameter
                                           ]
                ]
  , "vd" ~: bad [ FunctionDecl False TVoid [ FuncStub "f" VoidParameter
                                           , FuncStub "f" (Parameters [ScalarParam TInt "a"])
                                           ]
                ]
  , "vd" ~: bad [ FunctionDecl False TVoid [
                    FuncStub "f" (Parameters [ScalarParam TInt "a", ScalarParam TChar "a"])
                  ]
                ]
  ]
