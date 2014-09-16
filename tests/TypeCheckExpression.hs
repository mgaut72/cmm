import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Data.Map.Strict as M
-- import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Language.CMM.AST
import Language.CMM.TypeChecker.Expression

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


(|~?=) :: Expression -> TType -> Test
a |~?= b = runParser (typeOf a) initialState "" "" ~?= Right b

bad a = TestCase (unless (isLeft res) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where res = runParser (typeOf a) initialState "" ""
       isLeft (Right a) = False
       isLeft (Left a) = True

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b


tests = test
  [ "tChar1" ~: LitChar 'a' |~?= TChar
  , "tChar2" ~: LitChar '\0' |~?= TChar
  , "tStr1" ~: LitString "ac" |~?= TArray TChar
  , "tNeg" ~: bad $ Negative (LitString "ac")
  , "tNeg" ~: Negative (LitInt 1) |~?= TInt
  , "tNeg" ~: Negative (LitChar 'a') |~?= TInt
  , "tNeg" ~: Negative (Negative (LitInt 56)) |~?= TInt
  , "tBin" ~: Binary Plus (LitInt 1) (LitInt 2) |~?= TInt
  , "tBin" ~: Binary Plus (LitChar '1') (LitInt 2) |~?= TInt
  , "tBin" ~: bad $ Binary Plus (LitString "1") (LitChar '2')
  , "tRel" ~: Relative Leq (LitChar '1') (LitInt 2) |~?= TBool
  , "tRel" ~: bad $ Relative Leq (LitString "1") (LitInt 2)
  , "tNot" ~: Not (Relative Leq (LitChar '1') (LitInt 2)) |~?= TBool
  , "tNot" ~: bad $ Not (Relative Leq (LitString "1") (LitInt 2))
  , "tLog" ~: Logical And (Not (Relative Leq (LitChar '1') (LitInt 2))) (Not (Relative Leq (LitChar '1') (LitInt 2))) |~?= TBool
  , "tLog" ~: bad $ Logical And (Not (Relative Leq (LitChar '1') (LitInt 2))) (LitInt 1)
  , "tVar" ~: Var (Scalar "a") |~?= TInt
  , "tVar" ~: Var (Scalar "b") |~?= TChar
  , "tVar" ~: Binary Plus (Var (Scalar "a")) (LitChar 'a') |~?= TInt
  , "tVar" ~: Binary Plus (Var (Scalar "a")) (Var (Scalar "a")) |~?= TInt
  , "tVar" ~: Binary Plus (LitChar 'a') (Var (Scalar "a")) |~?= TInt
  , "tVar" ~: Binary Minus (Var (Scalar "a")) (Var (Scalar "b")) |~?= TInt
  , "tArr" ~: Var (Array "aa" (LitInt 1)) |~?= TInt
  , "tArr" ~: Var (Array "aa" (LitChar '1')) |~?= TInt
  , "tArr" ~: bad $ Var (Array "aa" (LitString "1"))
  , "tArr" ~: Var (Array "aa" (Binary Plus (LitInt 1) (LitInt 2))) |~?= TInt
  , "tArr" ~: Var (Array "bb" (Binary Plus (LitInt 1) (LitInt 2))) |~?= TChar
  , "tArr" ~: bad $ Var (Array "bb" (Binary Plus (LitInt 1) (LitString "2")))
  , "tFcn" ~: FunctionCall (Function "none" []) |~?= TInt
  , "tFcn" ~: bad $ FunctionCall (Function "none" [LitInt 1])
  , "tFcn" ~: FunctionCall (Function "one" [LitInt 1]) |~?= TInt
  , "tFcn" ~: bad $ FunctionCall (Function "one" [])
  , "tFcn" ~: bad $ FunctionCall (Function "one" [LitInt 1, LitChar '2'])
  , "tFcn" ~: FunctionCall (Function "two" [LitInt 1, LitInt 2]) |~?= TInt
  , "tFcn" ~: FunctionCall (Function "two" [LitInt 1, LitChar '2']) |~?= TInt
  , "tFcn" ~: bad $ FunctionCall (Function "two" [LitChar '2'])
  , "tFcn" ~: bad $ FunctionCall (Function "two" [LitString "1", LitChar '2'])
  ]
