import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Data.Map.Strict as M
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Language.CMM.AST
import Language.CMM.TypeChecker.Statement

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


a |~?= b = runParser (typeCheckStatement a) initialState "" "" ~?= Right b

bad a = TestCase (unless (isLeft res) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where res = runParser (typeCheckStatement a) initialState "" ""
       isLeft (Right a) = False
       isLeft (Left a) = True

good a = TestCase (unless (isRight res) (assertFailure ("expected good parse\ngot: " ++ show res)))
 where res = runParser (typeCheckStatement a) initialState "" ""
       isRight (Right a) = True
       isRight (Left a) = False

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b


tests = test
  [ "if" ~: good $ If (Relative Leq (LitChar '1') (LitInt 2)) None
  , "if" ~: good $ If (Logical And (Not (Relative Leq (LitChar '1') (LitInt 2))) (Not (Relative Leq (LitChar '1') (LitInt 2)))) None
  , "if" ~: bad  $ If (Binary Plus (LitChar '1') (LitInt 2)) None
  , "ife" ~: good $ IfElse (Relative Leq (LitChar '1') (LitInt 2)) None None
  , "ife" ~: good $ IfElse (Logical And (Not (Relative Leq (LitChar '1') (LitInt 2))) (Not (Relative Leq (LitChar '1') (LitInt 2)))) None None
  , "ife" ~: bad  $ IfElse (Binary Plus (LitChar '1') (LitInt 2)) None None
  , "while" ~: good $ While (Relative Leq (LitChar '1') (LitInt 2)) None
  , "while" ~: good $ While (Logical And (Not (Relative Leq (LitChar '1') (LitInt 2))) (Not (Relative Leq (LitChar '1') (LitInt 2)))) None
  , "while" ~: bad  $ While (Binary Plus (LitChar '1') (LitInt 2)) None
  , "return" ~: good $ Return Nothing
  , "return" ~: good $ Return (Just (LitInt 1))
  , "none" ~: good $ None
  ]
