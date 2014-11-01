import System.Exit
import Test.HUnit
import Text.Parsec
import Text.Parsec.Error
import Control.Monad
import Control.Monad.Writer
import Data.Map as M

import Language.CMM.AST
import Language.CMM.Parser.Base
import Language.CMM.Parser.UnTyped

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

readExpr = fst . runWriter . runParserT ep initialTables "compile"
 where ep = do
         whiteSpace
         e <- declarationP
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
  [ "vd1" ~: "int a;" |~?= VariableDecl (VarDecl TInt [Scalar "a"])
  , "vd1" ~: "int abc ; " |~?= VariableDecl (VarDecl TInt [Scalar "abc"])
  , "vd2" ~: "int ac,bc; " |~?= VariableDecl (VarDecl TInt [Scalar "ac", Scalar "bc"])
  , "vd2" ~: "int ac ,  bc ; " |~?= VariableDecl (VarDecl TInt [Scalar "ac", Scalar "bc"])
  , "vd3" ~: "int ac[10]; " |~?= VariableDecl (VarDecl TInt [Array "ac" (LitInt 10)])
  , "vd3" ~: "int ac  [ 10  ]  ; " |~?= VariableDecl (VarDecl TInt [Array "ac" (LitInt 10)])
  , "vd4" ~: "int arr [ 10  ], foo  ; "
        |~?= VariableDecl (VarDecl TInt [Array "arr" (LitInt 10), Scalar "foo"])
  , "vd4" ~: "int arr[10],foo;"
        |~?= VariableDecl (VarDecl TInt [Array "arr" (LitInt 10), Scalar "foo"])
  , "vd5" ~: bad "intarr[10],foo;"
  , "vd5" ~: bad "int arr[];"
  , "vd5" ~: bad "int arr[3]"
  , "vd5" ~: bad "int foo"
  , "vd5" ~: bad "int arr[3], \"asdf\""
  , "vd5" ~: bad "int arr[3], \"asdf\";"
  , "vd5" ~: bad "int foo = 5;"
  , "vd5" ~: bad "foo;"
  , "vd5" ~: bad "void foo;"
  , "vd1" ~: "char abc ; " |~?= VariableDecl (VarDecl TChar [Scalar "abc"])
  , "vd2" ~: "char ac,bc; " |~?= VariableDecl (VarDecl TChar [Scalar "ac", Scalar "bc"])
  , "vd2" ~: "char ac ,  bc ; " |~?= VariableDecl (VarDecl TChar [Scalar "ac", Scalar "bc"])
  , "vd3" ~: "char ac[10]; " |~?= VariableDecl (VarDecl TChar [Array "ac" (LitInt 10)])
  , "vd3" ~: "char ac  [ 10  ]  ; " |~?= VariableDecl (VarDecl TChar [Array "ac" (LitInt 10)])
  , "vd4" ~: "char arr [ 10  ], foo  ; "
        |~?= VariableDecl (VarDecl TChar [Array "arr" (LitInt 10), Scalar "foo"])
  , "vd4" ~: "char arr[10],foo;"
        |~?= VariableDecl (VarDecl TChar [Array "arr" (LitInt 10), Scalar "foo"])
  , "vd5" ~: bad "chararr[10],foo;"
  , "vd5" ~: bad "char arr[];"
  , "vd5" ~: bad "char arr[3]"
  , "vd5" ~: bad "char foo"
  , "vd5" ~: bad "char arr[3], \"asdf\""
  , "vd5" ~: bad "char arr[3], \"asdf\";"
  , "vd5" ~: bad "char foo = 5;"

  -- FunctionDecl
  , "fd1" ~: "int main(void);"
        |~?= FunctionDecl False TInt [FuncStub "main" VoidParameter]
  , "fd2" ~: "int main(int asdf);"
        |~?= FunctionDecl False TInt [FuncStub "main" (Parameters [ScalarParam TInt "asdf"])]
  , "fd3" ~: "int main(int asdf,int arr[],char car);"
        |~?= FunctionDecl False TInt
               [ FuncStub "main" (Parameters [ ScalarParam TInt  "asdf"
                                            , ArrayParam  TInt  "arr"
                                            , ScalarParam TChar "car"
                                            ])
               ]
  , "fd4" ~: "extern int main(void);"
        |~?= FunctionDecl True TInt [FuncStub "main" VoidParameter]
  , "fd5" ~: "extern int main(int asdf);"
        |~?= FunctionDecl True TInt [FuncStub "main" (Parameters [ScalarParam TInt "asdf"])]
  , "fd6" ~: "extern int main(int asdf,int arr[],char car);"
        |~?= FunctionDecl True TInt
               [ FuncStub "main" (Parameters [ ScalarParam TInt  "asdf"
                                            , ArrayParam  TInt  "arr"
                                            , ScalarParam TChar "car"
                                            ])
               ]
  , "vd1" ~: "void main(void);"
        |~?= FunctionDecl False TVoid [FuncStub "main" VoidParameter]
  , "vd2" ~: "void main(int asdf);"
        |~?= FunctionDecl False TVoid [FuncStub "main" (Parameters [ScalarParam TInt "asdf"])]
  , "vd3" ~: "void main(int asdf,int arr[],char car);"
        |~?= FunctionDecl False TVoid
              [ FuncStub "main" (Parameters [ ScalarParam TInt  "asdf"
                                            , ArrayParam  TInt  "arr"
                                            , ScalarParam TChar "car"
                                            ])
              ]
  , "vd4" ~: "extern void main(void);"
        |~?= FunctionDecl True TVoid [FuncStub "main" VoidParameter]
  , "vd5" ~: "extern void main(int asdf);"
        |~?= FunctionDecl True TVoid [FuncStub "main" (Parameters [ScalarParam TInt "asdf"])]
  , "vd6" ~: "extern void main(int asdf,int arr[],char car);"
        |~?= FunctionDecl True TVoid
               [ FuncStub "main" (Parameters [ ScalarParam TInt  "asdf"
                                            , ArrayParam  TInt  "arr"
                                            , ScalarParam TChar "car"
                                            ])
               ]
  ]

