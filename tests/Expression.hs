import System.Exit
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Monad
import Data.Map.Strict as M

import Language.CMM.Syntax.AST
import Language.CMM.Syntax.Parser.UnTyped
import Language.CMM.Syntax.Parser.Base
import Language.CMM.Syntax.TypeChecker

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

initialTables = Tables { _symbols = syms
                       , _functions = fcns
                       }

syms = M.fromList [ ("f", TInt), ("ident", TInt), ("a", TInt), ("f1", TInt)
                  , ("f0", TInt), ("f3", TInt), ("x", TInt), ("y", TInt)
                  ]
fcns = M.fromList [ ("f", [TInt, TInt]), ("f", [TInt, TInt]), ("f0", [])
                  , ("f1", [TInt]), ("f3", [TChar, TArray TChar, TInt])
                  ]

readExpr = runParser ep initialTables "compile"
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

instance (Eq ParseError) where
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
  , "testNegative4" ~: "-(-56)" |~?= Negative (Negative (LitInt 56))
  , "testNegative5" ~: "- ( - 56 )" |~?= Negative (Negative (LitInt 56))
  , "testNegative6" ~: bad "- ( - 5 6 )"
  , "testNegative7" ~: bad "-'56"
  , "testNot1" ~: "!(1==1)" |~?= Not (Relative Eq (LitInt 1) (LitInt 1))
  , "testNot1" ~: "! ( 1 == 1 ) " |~?= Not (Relative Eq (LitInt 1) (LitInt 1))
  , "testNot6" ~: bad "!'56"
  , "tMath1" ~: "1 + 2" |~?= Binary Plus (LitInt 1) (LitInt 2)
  , "tMath2" ~: "23 * 1 + 2" |~?= Binary Plus (Binary Times (LitInt 23) (LitInt 1)) (LitInt 2)
  , "tMath3" ~: "2 * (1 + 24)" |~?= Binary Times (LitInt 2) (Binary Plus (LitInt 1) (LitInt 24))
  , "tMath4" ~: "2 / 1 * 2" |~?= Binary Times (Binary Divide (LitInt 2) (LitInt 1)) (LitInt 2)
  , "tMath5" ~: "2 * 1 / 2" |~?= Binary Divide (Binary Times (LitInt 2) (LitInt 1)) (LitInt 2)
  , "tMath6" ~: "-1 + 2" |~?= Binary Plus (Negative (LitInt 1)) (LitInt 2)
  , "tMath7" ~: "-(1 + 2)" |~?= Negative (Binary Plus (LitInt 1) (LitInt 2))
  , "tFun1" ~: "f1('a')" |~?= FunctionCall (Function "f1" [LitChar 'a'])
  , "tFun1" ~: "f1 ( 'a' ) " |~?= FunctionCall (Function "f1" [LitChar 'a'])
  , "tFun2" ~: "f0()" |~?= FunctionCall (Function "f0" [])
  , "tFun3" ~: "f3('a', \"bcd\", 1)" |~?= FunctionCall (Function "f3" [LitChar 'a', LitString "bcd", LitInt 1])
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
