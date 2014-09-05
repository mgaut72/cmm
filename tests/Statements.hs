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
         e <- statementP
         whiteSpace
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
  [ "return1" ~: "return;" |~?= Return Nothing
  , "return2" ~: "return  ; " |~?= Return Nothing
  , "return3" ~: "return 1; " |~?= Return (Just (LitInt 1))
  , "return4" ~: bad "retur 1; "
  , "return5" ~: bad "return1; "
  , "return6" ~: bad "return1 ;"
  , "return7" ~: bad "return 1"
  , "return7" ~: bad "re turn 1;"
  , "if1" ~: "if(1) return; " |~?= If (LitInt 1) (Return Nothing)
  , "if2" ~: "if(1) return 1; " |~?= If (LitInt 1) (Return (Just (LitInt 1)))
  , "if2" ~: "if (1) return 1; " |~?= If (LitInt 1) (Return (Just (LitInt 1)))
  , "if2" ~: "if (1) return 1; " |~?= If (LitInt 1) (Return (Just (LitInt 1)))
  , "if2" ~: "if ( 1  )   return 1; " |~?= If (LitInt 1) (Return (Just (LitInt 1)))
  , "if3" ~: "if ( (1 + 1)  )   return 1; " |~?= If (Binary Plus (LitInt 1) (LitInt 1)) (Return (Just (LitInt 1)))
  , "if3" ~: "if ( 1 + 1  )   return 1; " |~?= If (Binary Plus (LitInt 1) (LitInt 1)) (Return (Just (LitInt 1)))
  , "if3" ~: "if ( 1 + 1  )\n\treturn 1; " |~?= If (Binary Plus (LitInt 1) (LitInt 1)) (Return (Just (LitInt 1)))
  , "if4" ~: bad "if ( 1 + 1  ))   return 1; "
  , "if4" ~: bad "if (( 1 + 1  )   return 1; "
  , "if5" ~: bad "if  1 + 1    return 1; "
  , "ifelse1" ~: "if (1) return 1; else return;" |~?= IfElse (LitInt 1) (Return (Just (LitInt 1))) (Return (Nothing))
  , "ifelse1" ~: "if (1) return 1;else return;" |~?= IfElse (LitInt 1) (Return (Just (LitInt 1))) (Return (Nothing))
  , "ifelse1" ~: "if (1)\n\treturn 1;\nelse\n\treturn;" |~?= IfElse (LitInt 1) (Return (Just (LitInt 1))) (Return (Nothing))
  , "ifelse2" ~: "if (1) if(2) return 1;\nelse\n\treturn;" |~?= If (LitInt 1) (IfElse (LitInt 2) (Return (Just (LitInt 1))) (Return Nothing))
  , "ifelse2" ~: "if (1) if(2) return 1;\nelse\n\treturn; else return 3;" |~?=
                  IfElse (LitInt 1)
                         (IfElse (LitInt 2)
                                 (Return (Just (LitInt 1)))
                                 (Return Nothing))
                         (Return (Just (LitInt 3)))
  , "assign1" ~: "a = 1;" |~?= Assign (Assignment (Scalar "a") (LitInt 1))
  , "assign2" ~: "a[1+2] = f(x\t)\n;" |~?= Assign (Assignment (Array "a" (Binary Plus (LitInt 1) (LitInt 2))) (FunctionCall "f" [Var (Scalar "x")]))
  ]
