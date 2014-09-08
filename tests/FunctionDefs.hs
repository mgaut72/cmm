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

readExpr s = parse (ep) "testParse" s
 where ep = do
         whiteSpace
         e <- functionDefP
         eof
         return e

a |~?= b = readExpr a ~?= Right b
bad a = TestCase (unless (isLeft res) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where res = readExpr a
       isLeft (Right a) = False
       isLeft (Left a) = True

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b

tests = TestList [ TestLabel "function" f1
                 , TestLabel "noVarDecs" f2
                 , TestLabel "noStatements" f3
                 , TestLabel "empty body" f4
                 ]

f1 = unlines [ "int main(int argc, char argv[]) {"
             , "\tint i, j  , k;  "
             , "\tint results[10 ]     ; " 
             -- end declarations
             , "\tfor(i = 0; i < 10; i = i + 1){"
             , "\t\tresults[i] = i;"
             , "\t}"
             , "\treturn 0;"
             -- end statements
             , "}"
             ]
    |~?= FunctionDef Int "main" (Parameters [ScalarParam Int "argc", ArrayParam Char "argv"])
                     [ VarDecl Int [Scalar "i", Scalar "j", Scalar "k"]
                     , VarDecl Int [Array "results" (LitInt 10)]
                     ]
                     [ For (Just (Assignment (Scalar "i") (LitInt 0)))
                               (Just (Relative Less (Var (Scalar "i")) (LitInt 10)))
                               (Just (Assignment (Scalar "i") (Binary Plus (Var (Scalar "i")) (LitInt 1))))
                           (Bracketed [Assign (Assignment (Array "results" (Var (Scalar "i"))) (Var (Scalar "i")))])
                     , Return (Just (LitInt 0))
                     ]

f2 = unlines [ "int main(int argc, char argv[]) {"
             , "\tfor(i = 0; i < 10; i = i + 1){"
             , "\t\tresults[i] = i;"
             , "\t}"
             , "\treturn 0;"
             -- end statements
             , "}"
             ]
    |~?= FunctionDef Int "main" (Parameters [ScalarParam Int "argc", ArrayParam Char "argv"])
                     []
                     [ For (Just (Assignment (Scalar "i") (LitInt 0)))
                               (Just (Relative Less (Var (Scalar "i")) (LitInt 10)))
                               (Just (Assignment (Scalar "i") (Binary Plus (Var (Scalar "i")) (LitInt 1))))
                           (Bracketed [Assign (Assignment (Array "results" (Var (Scalar "i"))) (Var (Scalar "i")))])
                     , Return (Just (LitInt 0))
                     ]

f3 = unlines [ "int main(int argc, char argv[]) {"
             , "\tint i, j  , k;  "
             , "\tint results[10 ]     ; " 
             -- end statements
             , "}"
             ]
    |~?= FunctionDef Int "main" (Parameters [ScalarParam Int "argc", ArrayParam Char "argv"])
                     [ VarDecl Int [Scalar "i", Scalar "j", Scalar "k"]
                     , VarDecl Int [Array "results" (LitInt 10)]
                     ]
                     []


f4 = unlines [ "int main(int argc, char argv[]) {"
             , "}"
             ]
    |~?= FunctionDef Int "main" (Parameters [ScalarParam Int "argc", ArrayParam Char "argv"])
                     [] []

