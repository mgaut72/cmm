import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Error

import Language.CMM.AST
import Language.CMM.TypeChecker.Assignment

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

initialState :: Tables
initialState = Tables { _globalSymbols = M.fromList symbolList
                      , _localSymbols = M.empty
                      , _functions = M.fromList fcnList
                      , _currentFunctionType = TVoid
                      }

symbolList = [ ("a", TInt), ("b", TChar), ("none", TInt)
             , ("one", TInt), ("two", TInt), ("aa", TArray TInt)
             , ("bb", TArray TChar)
             ]
fcnList = [("none", []), ("one", [TInt]), ("two", [TInt, TChar])]


readA a = runWriter . runParserT (typeCheckAssignment a) initialState "" $ ""

bad a = TestCase (when (null errs) (assertFailure ("expected bad parse\ngot: " ++ show x)))
 where x@(res, errs) = readA a
       isLeft (Right _) = False
       isLeft (Left _) = True

good a = TestCase (unless (null errs) (assertFailure ("expected good parse\ngot: " ++ show x)))
 where x@(res, errs) = readA a
       isRight (Right _) = True
       isRight (Left _) = False

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b


tests = test
  [ "tVar" ~: good $ Assignment (Scalar "a") (LitInt 1)
  , "tVar" ~: good $ Assignment (Scalar "b") (LitChar 'a')
  , "tVar" ~: good $ Assignment (Scalar "a") (LitChar 'a')
  , "tVar" ~: good $ Assignment (Scalar "b") (LitInt 1)
  , "tVar" ~: good $ Assignment (Array "bb" (LitInt 1)) (LitChar '1')
  , "tVar" ~: good $ Assignment (Array "bb" (LitInt 1)) (LitInt 1)
  , "tVar" ~: bad  $ Assignment (Scalar "bb") (LitInt 1)
  , "tVar" ~: bad  $ Assignment (Scalar "bb") (LitString "1")
  ]
