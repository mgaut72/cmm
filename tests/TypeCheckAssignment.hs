import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Data.Map.Strict as M
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Language.CMM.AST
import Language.CMM.TypeChecker.Assignment

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

initialState :: Tables
initialState = Tables { _symbols = M.fromList symbolList
                      , _functions = M.fromList fcnList
                      }

symbolList = [ ("a", TInt), ("b", TChar), ("none", TInt)
             , ("one", TInt), ("two", TInt), ("aa", TArray TInt)
             , ("bb", TArray TChar)
             ]
fcnList = [("none", []), ("one", [TInt]), ("two", [TInt, TChar])]


a |~?= b = runParser (typeCheckAssignment a) initialState "" "" ~?= Right b

bad a = TestCase (unless (isLeft res) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where res = runParser (typeCheckAssignment a) initialState "" ""
       isLeft (Right a) = False
       isLeft (Left a) = True

good a = TestCase (unless (isRight res) (assertFailure ("expected good parse\ngot: " ++ show res)))
 where res = runParser (typeCheckAssignment a) initialState "" ""
       isRight (Right a) = True
       isRight (Left a) = False

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
