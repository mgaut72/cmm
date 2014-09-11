import System.Exit
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Monad

import Language.CMM.Syntax.AST
import Language.CMM.Syntax.Parser

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

readExpr = parse ep "testParse"
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
  [ "vd1" ~: "int a;" |~?= VariableDecl (VarDecl Int [Scalar "a"])
  , "vd1" ~: "int abc ; " |~?= VariableDecl (VarDecl Int [Scalar "abc"])
  , "vd2" ~: "int ac,bc; " |~?= VariableDecl (VarDecl Int [Scalar "ac", Scalar "bc"])
  , "vd2" ~: "int ac ,  bc ; " |~?= VariableDecl (VarDecl Int [Scalar "ac", Scalar "bc"])
  , "vd3" ~: "int ac[10]; " |~?= VariableDecl (VarDecl Int [Array "ac" (LitInt 10)])
  , "vd3" ~: "int ac  [ 10  ]  ; " |~?= VariableDecl (VarDecl Int [Array "ac" (LitInt 10)])
  , "vd4" ~: "int arr [ 10  ], foo  ; "
        |~?= VariableDecl (VarDecl Int [Array "arr" (LitInt 10), Scalar "foo"])
  , "vd4" ~: "int arr[10],foo;"
        |~?= VariableDecl (VarDecl Int [Array "arr" (LitInt 10), Scalar "foo"])
  , "vd5" ~: bad "intarr[10],foo;"
  , "vd5" ~: bad "int arr[];"
  , "vd5" ~: bad "int arr[3]"
  , "vd5" ~: bad "int foo"
  , "vd5" ~: bad "int arr[3], \"asdf\""
  , "vd5" ~: bad "int arr[3], \"asdf\";"
  , "vd5" ~: bad "int foo = 5;"
  , "vd5" ~: bad "foo;"
  , "vd5" ~: bad "void foo;"
  , "vd1" ~: "char abc ; " |~?= VariableDecl (VarDecl Char [Scalar "abc"])
  , "vd2" ~: "char ac,bc; " |~?= VariableDecl (VarDecl Char [Scalar "ac", Scalar "bc"])
  , "vd2" ~: "char ac ,  bc ; " |~?= VariableDecl (VarDecl Char [Scalar "ac", Scalar "bc"])
  , "vd3" ~: "char ac[10]; " |~?= VariableDecl (VarDecl Char [Array "ac" (LitInt 10)])
  , "vd3" ~: "char ac  [ 10  ]  ; " |~?= VariableDecl (VarDecl Char [Array "ac" (LitInt 10)])
  , "vd4" ~: "char arr [ 10  ], foo  ; "
        |~?= VariableDecl (VarDecl Char [Array "arr" (LitInt 10), Scalar "foo"])
  , "vd4" ~: "char arr[10],foo;"
        |~?= VariableDecl (VarDecl Char [Array "arr" (LitInt 10), Scalar "foo"])
  , "vd5" ~: bad "chararr[10],foo;"
  , "vd5" ~: bad "char arr[];"
  , "vd5" ~: bad "char arr[3]"
  , "vd5" ~: bad "char foo"
  , "vd5" ~: bad "char arr[3], \"asdf\""
  , "vd5" ~: bad "char arr[3], \"asdf\";"
  , "vd5" ~: bad "char foo = 5;"

  -- FunctionDecl
  , "fd1" ~: "int main(void);"
        |~?= FunctionDecl False Int [FuncStub "main" VoidParameter]
  , "fd2" ~: "int main(int asdf);"
        |~?= FunctionDecl False Int [FuncStub "main" (Parameters [ScalarParam Int "asdf"])]
  , "fd3" ~: "int main(int asdf,int arr[],char car);"
        |~?= FunctionDecl False Int
               [ FuncStub "main" (Parameters [ ScalarParam Int  "asdf"
                                            , ArrayParam  Int  "arr"
                                            , ScalarParam Char "car"
                                            ])
               ]
  , "fd4" ~: "extern int main(void);"
        |~?= FunctionDecl True Int [FuncStub "main" VoidParameter]
  , "fd5" ~: "extern int main(int asdf);"
        |~?= FunctionDecl True Int [FuncStub "main" (Parameters [ScalarParam Int "asdf"])]
  , "fd6" ~: "extern int main(int asdf,int arr[],char car);"
        |~?= FunctionDecl True Int
               [ FuncStub "main" (Parameters [ ScalarParam Int  "asdf"
                                            , ArrayParam  Int  "arr"
                                            , ScalarParam Char "car"
                                            ])
               ]
  , "vd1" ~: "void main(void);"
        |~?= FunctionDecl False Void [FuncStub "main" VoidParameter]
  , "vd2" ~: "void main(int asdf);"
        |~?= FunctionDecl False Void [FuncStub "main" (Parameters [ScalarParam Int "asdf"])]
  , "vd3" ~: "void main(int asdf,int arr[],char car);"
        |~?= FunctionDecl False Void
              [ FuncStub "main" (Parameters [ ScalarParam Int  "asdf"
                                            , ArrayParam  Int  "arr"
                                            , ScalarParam Char "car"
                                            ])
              ]
  , "vd4" ~: "extern void main(void);"
        |~?= FunctionDecl True Void [FuncStub "main" VoidParameter]
  , "vd5" ~: "extern void main(int asdf);"
        |~?= FunctionDecl True Void [FuncStub "main" (Parameters [ScalarParam Int "asdf"])]
  , "vd6" ~: "extern void main(int asdf,int arr[],char car);"
        |~?= FunctionDecl True Void
               [ FuncStub "main" (Parameters [ ScalarParam Int  "asdf"
                                            , ArrayParam  Int  "arr"
                                            , ScalarParam Char "car"
                                            ])
               ]
  ]

