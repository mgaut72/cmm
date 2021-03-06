import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Error

import Language.CMM.AST
import Language.CMM.TypeChecker.Expression

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

initialState :: Tables
initialState = initialTables { _globalSymbols = M.fromList symbolList
                             , _functions = M.fromList fcnList
                             }

symbolList = [ ("a", TInt), ("b", TChar), ("none", TInt)
             , ("one", TInt), ("two", TInt), ("aa", TArray TInt (Just 2))
             , ("bb", TArray TChar (Just 2))
             , ("voidF", TVoid)
             ]
fcnList = [ ("none", []), ("voidF", []), ("one", [TInt])
          , ("two", [TInt, TChar])
          ]


a |~?= b = readE "" ~?= (Right b, [])
 where readE = runWriter . runParserT (typeOf a) initialState ""

bad a = TestCase (when (null errs) (assertFailure ("expected bad parse\ngot: " ++ show x)))
 where x@(res, errs) = runWriter . runParserT (typeOf a) initialState "" $ ""
       isLeft (Right a) = False
       isLeft (Left a) = True

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b


tests = test
  [ "tChar1" ~: LitChar 'a' |~?= TChar
  , "tChar2" ~: LitChar '\0' |~?= TChar
  , "tStr1" ~: LitString "ac" |~?= TArray TChar Nothing
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

  -- function called from withinn an expression must not have void return type
  , "tFcn" ~: bad $ FunctionCall (Function "voidF" [])

  -- function name cannot be a variable
  , "tVar" ~: bad $ Var (Scalar "none")
  ]
