import System.Exit
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Monad
import Data.Map.Strict as M

import Language.CMM.Syntax.AST
import Language.CMM.Syntax.Parser.Base
import Language.CMM.Syntax.Parser.UnTyped

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

initialTables = Tables { _symbols = M.empty
                       , _functions = M.empty
                       }

readExpr = runParser ep initialTables "compile"
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
                 , TestLabel "no parameters" f5
                 , TestLabel "bad no parameters" f6
                 , TestLabel "void function" v1
                 , TestLabel "void noVarDecs" v2
                 , TestLabel "void noStatements" v3
                 , TestLabel "void empty body" v4
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

f5 = unlines [ "int main(void) {"
             , "}"
             ]
    |~?= FunctionDef Int "main" VoidParameter [] []

f6 = bad $ unlines [ "int main() {"
                   , "}"
                   ]


v1 = unlines [ "void main(int argc, char argv[]) {"
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
    |~?= FunctionDef Void "main" (Parameters [ScalarParam Int "argc", ArrayParam Char "argv"])
                     [ VarDecl Int [Scalar "i", Scalar "j", Scalar "k"]
                     , VarDecl Int [Array "results" (LitInt 10)]
                     ]
                     [ For (Just (Assignment (Scalar "i") (LitInt 0)))
                               (Just (Relative Less (Var (Scalar "i")) (LitInt 10)))
                               (Just (Assignment (Scalar "i") (Binary Plus (Var (Scalar "i")) (LitInt 1))))
                           (Bracketed [Assign (Assignment (Array "results" (Var (Scalar "i"))) (Var (Scalar "i")))])
                     , Return (Just (LitInt 0))
                     ]

v2 = unlines [ "void main(int argc, char argv[]) {"
             , "\tfor(i = 0; i < 10; i = i + 1){"
             , "\t\tresults[i] = i;"
             , "\t}"
             , "\treturn 0;"
             -- end statements
             , "}"
             ]
    |~?= FunctionDef Void "main" (Parameters [ScalarParam Int "argc", ArrayParam Char "argv"])
                     []
                     [ For (Just (Assignment (Scalar "i") (LitInt 0)))
                               (Just (Relative Less (Var (Scalar "i")) (LitInt 10)))
                               (Just (Assignment (Scalar "i") (Binary Plus (Var (Scalar "i")) (LitInt 1))))
                           (Bracketed [Assign (Assignment (Array "results" (Var (Scalar "i"))) (Var (Scalar "i")))])
                     , Return (Just (LitInt 0))
                     ]

v3 = unlines [ "void main(int argc, char argv[]) {"
             , "\tint i, j  , k;  "
             , "\tint results[10 ]     ; " 
             -- end statements
             , "}"
             ]
    |~?= FunctionDef Void "main" (Parameters [ScalarParam Int "argc", ArrayParam Char "argv"])
                     [ VarDecl Int [Scalar "i", Scalar "j", Scalar "k"]
                     , VarDecl Int [Array "results" (LitInt 10)]
                     ]
                     []


v4 = unlines [ "void main(int argc, char argv[]) {"
             , "}"
             ]
    |~?= FunctionDef Void "main" (Parameters [ScalarParam Int "argc", ArrayParam Char "argv"])
                     [] []

