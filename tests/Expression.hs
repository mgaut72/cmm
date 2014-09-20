import System.Exit
import Test.HUnit
import Text.Parsec
import Text.Parsec.Error
import Control.Monad
import Control.Monad.Writer
import Data.Map.Strict as M

import Language.CMM.AST
import Language.CMM.Parser.UnTyped
import Language.CMM.Parser.Base

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

readExpr = fst . runWriter . runParserT ep initialTables "compile"
 where ep = do
         whiteSpace
         e <- expressionP
         eof
         return e

a |~?= b = readExpr a ~?= Right b
bad a = TestCase (unless (isLeft res) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where res = readExpr a
       isLeft (Right a) = False
       isLeft (Left a) = True

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

tests = test
  [ "tChar1" ~: "'a'" |~?= LitChar 'a'
  , "tChar2" ~: "'\\n'" |~?= LitChar '\n'
  , "tChar2" ~: "'\\n'  " |~?= LitChar '\n'
  , "tChar3" ~: "'\\0'" |~?= LitChar '\0'
  , "tChar4" ~: bad "'a"
  , "tChar5" ~: bad "'ab'"
  , "tStr1" ~: "\"abc\"" |~?= LitString "abc"
  , "tStr1" ~: "\" abc \"  " |~?= LitString " abc "
  , "tStr2" ~: bad "\"abc"
  , "tStr3" ~: "\"a\\nbc\"" |~?= LitString "a\nbc"
  , "testNegative1" ~: "-1" |~?= Negative (LitInt 1)
  , "testNegative1" ~: " - 1 " |~?= Negative (LitInt 1)
  , "testNegative2" ~: "-'a'" |~?= Negative (LitChar 'a')
  , "testNegative3" ~: bad "--2"
  , "testNegative4" ~: "-\"ab\"" |~?= Negative (LitString "ab")
  , "testNegative5" ~: "-(-56)" |~?= Negative (Negative (LitInt 56))
  , "testNegative5" ~: "- ( - 56 )" |~?= Negative (Negative (LitInt 56))
  , "testNegative5" ~: bad "- ( - 5 6 )"
  , "testNegative6" ~: bad "-'56"
  , "testNot1" ~: "!1" |~?= Not (LitInt 1)
  , "testNot1" ~: "! 1 " |~?= Not (LitInt 1)
  , "testNot2" ~: "!'a'" |~?= Not (LitChar 'a')
  , "testNot3" ~: bad "!-2"
  , "testNot4" ~: "!\"ab\"" |~?= Not (LitString "ab")
  , "testNot5" ~: "!(!56)" |~?= Not (Not (LitInt 56))
  , "testNot6" ~: bad "!'56"
  , "testNotNeg1" ~: bad "!-1"
  , "testNotNeg2" ~: bad "-!1"
  , "testNotNeg3" ~: "-(!1)" |~?= Negative (Not (LitInt 1))
  , "testNotNeg3" ~: "- ( ! 1 )" |~?= Negative (Not (LitInt 1))
  , "testNotNeg4" ~: "!(-1)" |~?= Not (Negative (LitInt 1))
  , "tMath1" ~: "1 + 2" |~?= Binary Plus (LitInt 1) (LitInt 2)
  , "tMath2" ~: "23 * 1 + 2" |~?= Binary Plus (Binary Times (LitInt 23) (LitInt 1)) (LitInt 2)
  , "tMath3" ~: "2 * (1 + 24)" |~?= Binary Times (LitInt 2) (Binary Plus (LitInt 1) (LitInt 24))
  , "tMath4" ~: "2 / 1 * 2" |~?= Binary Times (Binary Divide (LitInt 2) (LitInt 1)) (LitInt 2)
  , "tMath5" ~: "2 * 1 / 2" |~?= Binary Divide (Binary Times (LitInt 2) (LitInt 1)) (LitInt 2)
  , "tMath6" ~: "-1 + 2" |~?= Binary Plus (Negative (LitInt 1)) (LitInt 2)
  , "tMath7" ~: "-(1 + 2)" |~?= Negative (Binary Plus (LitInt 1) (LitInt 2))
  , "tFun1" ~: "f('a')" |~?= FunctionCall (Function "f" [LitChar 'a'])
  , "tFun1" ~: "f ( 'a' ) " |~?= FunctionCall (Function "f" [LitChar 'a'])
  , "tFun2" ~: "f()" |~?= FunctionCall (Function "f" [])
  , "tFun3" ~: "f('a', \"bcd\", 1)" |~?= FunctionCall (Function "f" [LitChar 'a', LitString "bcd", LitInt 1])
  , "tFun5" ~: bad "f('a', \"bcd\" 1)"
  , "tFun6" ~: bad "f('a', \"bcd\",, 1)"
  , "tFun7" ~: bad "f('a', \"bcd\"1"
  , "tArr1" ~: "f[1]" |~?= Var (Array "f" (LitInt 1))
  , "tArr1" ~: "f [ 1 ] " |~?= Var (Array "f" (LitInt 1))
  , "tArr2" ~: "f[1+2]" |~?= Var (Array "f" (Binary Plus (LitInt 1) (LitInt 2)))
  , "tArr3" ~: bad "f[]"
  , "tVar1" ~: "ident" |~?= Var (Scalar "ident")
  , "tVar1" ~: "ident\n" |~?= Var (Scalar "ident")
  , "tVar2" ~: bad "1dent"
  , "tComplex1" ~: "f(x,y) + 1" |~?= Binary Plus (FunctionCall (Function "f" [Var (Scalar "x"), Var (Scalar "y")])) (LitInt 1)
  , "tComplex1" ~: "f ( x , y  )+   1" |~?= Binary Plus (FunctionCall (Function "f" [Var (Scalar "x"), Var (Scalar "y")])) (LitInt 1)
  , "tComplex2" ~: "f(x,y + 1)" |~?= FunctionCall (Function "f" [Var (Scalar "x"), Binary Plus (Var (Scalar "y")) (LitInt 1)])
  , "tComplex3" ~: "f(x,(y + 1) * 3)" |~?= FunctionCall (Function "f" [Var (Scalar "x"), Binary Times (Binary Plus (Var (Scalar "y")) (LitInt 1)) (LitInt 3)])
  , "tComplex4" ~: "a[f(x,(y + 1) * 3)]" |~?= Var (Array "a" (FunctionCall (Function "f" [Var (Scalar "x"), Binary Times (Binary Plus (Var (Scalar "y")) (LitInt 1)) (LitInt 3)])))
  , "tComplex4" ~: "a   [ f(x   ,  (y + 1) * 3 ) ]" |~?= Var (Array "a" (FunctionCall (Function "f" [Var (Scalar "x"), Binary Times (Binary Plus (Var (Scalar "y")) (LitInt 1)) (LitInt 3)])))
  , "tComplex5" ~: bad "af(x,(y + 1) * 3)]"
  , "tComplex6" ~: bad "a[f(x,y + 1) * 3)]"
  ]
