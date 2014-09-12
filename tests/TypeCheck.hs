import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Data.Map.Strict as M

import Language.CMM.Syntax.AST
import Language.CMM.Syntax.TypeChecker

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
             , ("one", TInt), ("two", TInt)]
fcnList = [("none", []), ("one", [TInt]), ("two", [TInt, TChar])]


(|~?=) :: Expression -> TType -> Test
a |~?= b = evalState (typeOf a) initialState ~?= Right b

bad a = TestCase (unless (isLeft res) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where res = evalState (typeOf a) initialState
       isLeft (Right a) = False
       isLeft (Left a) = True



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
  , "tArr" ~: Var (Array "a" (LitInt 1)) |~?= TInt
  , "tArr" ~: Var (Array "a" (LitChar '1')) |~?= TInt
  , "tArr" ~: bad $ Var (Array "a" (LitString "1"))
  , "tArr" ~: Var (Array "a" (Binary Plus (LitInt 1) (LitInt 2))) |~?= TInt
  , "tArr" ~: Var (Array "b" (Binary Plus (LitInt 1) (LitInt 2))) |~?= TChar
  , "tArr" ~: bad $ Var (Array "b" (Binary Plus (LitInt 1) (LitString "2")))
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

--  , "tChar2" ~: "'\\n'" |~?= LitChar '\n'
--  , "tChar2" ~: "'\\n'  " |~?= LitChar '\n'
--  , "tChar3" ~: "'\\0'" |~?= LitChar '\0'
--  , "tChar4" ~: bad "'a"
--  , "tChar5" ~: bad "'ab'"
--  , "tStr1" ~: "\"abc\"" |~?= LitString "abc"
--  , "tStr1" ~: "\" abc \"  " |~?= LitString " abc "
--  , "tStr2" ~: bad "\"abc"
--  , "tStr3" ~: "\"a\\nbc\"" |~?= LitString "a\nbc"
--  , "testNegative1" ~: "-1" |~?= Negative (LitInt 1)
--  , "testNegative1" ~: " - 1 " |~?= Negative (LitInt 1)
--  , "testNegative2" ~: "-'a'" |~?= Negative (LitChar 'a')
--  , "testNegative3" ~: bad "--2"
--  , "testNegative4" ~: "-\"ab\"" |~?= Negative (LitString "ab")
--  , "testNegative5" ~: "-(-56)" |~?= Negative (Negative (LitInt 56))
--  , "testNegative5" ~: "- ( - 56 )" |~?= Negative (Negative (LitInt 56))
--  , "testNegative5" ~: bad "- ( - 5 6 )"
--  , "testNegative6" ~: bad "-'56"
--  , "testNot1" ~: "!1" |~?= Not (LitInt 1)
--  , "testNot1" ~: "! 1 " |~?= Not (LitInt 1)
--  , "testNot2" ~: "!'a'" |~?= Not (LitChar 'a')
--  , "testNot3" ~: bad "!-2"
--  , "testNot4" ~: "!\"ab\"" |~?= Not (LitString "ab")
--  , "testNot5" ~: "!(!56)" |~?= Not (Not (LitInt 56))
--  , "testNot6" ~: bad "!'56"
--  , "testNotNeg1" ~: bad "!-1"
--  , "testNotNeg2" ~: bad "-!1"
--  , "testNotNeg3" ~: "-(!1)" |~?= Negative (Not (LitInt 1))
--  , "testNotNeg3" ~: "- ( ! 1 )" |~?= Negative (Not (LitInt 1))
--  , "testNotNeg4" ~: "!(-1)" |~?= Not (Negative (LitInt 1))
--  , "tMath1" ~: "1 + 2" |~?= Binary Plus (LitInt 1) (LitInt 2)
--  , "tMath2" ~: "23 * 1 + 2" |~?= Binary Plus (Binary Times (LitInt 23) (LitInt 1)) (LitInt 2)
--  , "tMath3" ~: "2 * (1 + 24)" |~?= Binary Times (LitInt 2) (Binary Plus (LitInt 1) (LitInt 24))
--  , "tMath4" ~: "2 / 1 * 2" |~?= Binary Times (Binary Divide (LitInt 2) (LitInt 1)) (LitInt 2)
--  , "tMath5" ~: "2 * 1 / 2" |~?= Binary Divide (Binary Times (LitInt 2) (LitInt 1)) (LitInt 2)
--  , "tMath6" ~: "-1 + 2" |~?= Binary Plus (Negative (LitInt 1)) (LitInt 2)
--  , "tMath7" ~: "-(1 + 2)" |~?= Negative (Binary Plus (LitInt 1) (LitInt 2))
--  , "tFun1" ~: "f('a')" |~?= FunctionCall (Function "f" [LitChar 'a'])
--  , "tFun1" ~: "f ( 'a' ) " |~?= FunctionCall (Function "f" [LitChar 'a'])
--  , "tFun2" ~: "f()" |~?= FunctionCall (Function "f" [])
--  , "tFun3" ~: "f('a', \"bcd\", 1)" |~?= FunctionCall (Function "f" [LitChar 'a', LitString "bcd", LitInt 1])
--  , "tFun5" ~: bad "f('a', \"bcd\" 1)"
--  , "tFun6" ~: bad "f('a', \"bcd\",, 1)"
--  , "tFun7" ~: bad "f('a', \"bcd\"1"
--  , "tArr1" ~: "f[1]" |~?= Var (Array "f" (LitInt 1))
--  , "tArr1" ~: "f [ 1 ] " |~?= Var (Array "f" (LitInt 1))
--  , "tArr2" ~: "f[1+2]" |~?= Var (Array "f" (Binary Plus (LitInt 1) (LitInt 2)))
--  , "tArr3" ~: bad "f[]"
--  , "tVar1" ~: "ident" |~?= Var (Scalar "ident")
--  , "tVar1" ~: "ident\n" |~?= Var (Scalar "ident")
--  , "tVar2" ~: bad "1dent"
--  , "tComplex1" ~: "f(x,y) + 1" |~?= Binary Plus (FunctionCall (Function "f" [Var (Scalar "x"), Var (Scalar "y")])) (LitInt 1)
--  , "tComplex1" ~: "f ( x , y  )+   1" |~?= Binary Plus (FunctionCall (Function "f" [Var (Scalar "x"), Var (Scalar "y")])) (LitInt 1)
--  , "tComplex2" ~: "f(x,y + 1)" |~?= FunctionCall (Function "f" [Var (Scalar "x"), Binary Plus (Var (Scalar "y")) (LitInt 1)])
--  , "tComplex3" ~: "f(x,(y + 1) * 3)" |~?= FunctionCall (Function "f" [Var (Scalar "x"), Binary Times (Binary Plus (Var (Scalar "y")) (LitInt 1)) (LitInt 3)])
--  , "tComplex4" ~: "a[f(x,(y + 1) * 3)]" |~?= Var (Array "a" (FunctionCall (Function "f" [Var (Scalar "x"), Binary Times (Binary Plus (Var (Scalar "y")) (LitInt 1)) (LitInt 3)])))
--  , "tComplex4" ~: "a   [ f(x   ,  (y + 1) * 3 ) ]" |~?= Var (Array "a" (FunctionCall (Function "f" [Var (Scalar "x"), Binary Times (Binary Plus (Var (Scalar "y")) (LitInt 1)) (LitInt 3)])))
--  , "tComplex5" ~: bad "af(x,(y + 1) * 3)]"
--  , "tComplex6" ~: bad "a[f(x,y + 1) * 3)]"
--  ]
