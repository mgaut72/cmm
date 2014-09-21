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
         e <- statementP
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
  , "return3" ~: "return 1 ; " |~?= Return (Just (LitInt 1))
  , "return4" ~: bad "retur 1; "
  , "return5" ~: bad "return1; "
  , "return6" ~: bad "return1 ;"
  , "return7" ~: bad "return 1"
  , "return7" ~: bad "re turn 1;"
  , "if1" ~: "if(1) return; " |~?= If (LitInt 1) (Return Nothing)
  , "if1" ~: "if ( 1 ) return ; " |~?= If (LitInt 1) (Return Nothing)
  , "if2" ~: "if(1) return 1; " |~?= If (LitInt 1) (Return (Just (LitInt 1)))
  , "if2" ~: "if (1) return 1; " |~?= If (LitInt 1) (Return (Just (LitInt 1)))
  , "if2" ~: "if (1) return 1 ; " |~?= If (LitInt 1) (Return (Just (LitInt 1)))
  , "if2" ~: "if ( 1  )   return 1; " |~?= If (LitInt 1) (Return (Just (LitInt 1)))
  , "if3" ~: "if ( (1 + 1)  )   return 1; " |~?= If (Binary Plus (LitInt 1) (LitInt 1)) (Return (Just (LitInt 1)))
  , "if3" ~: "if ( 1 + 1  )   return 1; " |~?= If (Binary Plus (LitInt 1) (LitInt 1)) (Return (Just (LitInt 1)))
  , "if3" ~: "if ( 1 + 1  )\n\treturn 1; " |~?= If (Binary Plus (LitInt 1) (LitInt 1)) (Return (Just (LitInt 1)))
  , "if4" ~: bad "if ( 1 + 1  ))   return 1; "
  , "if4" ~: "if (( 1 + 1  )   return 1; " |~?= If ErrorE (Return (Just (LitInt 1)))
  , "if5" ~: bad "if  1 + 1    return 1; "
  , "if5" ~: "if(  1  1 )   return 1; " |~?= If ErrorE (Return (Just (LitInt 1)))
  , "ifelse1" ~: "if (1) return 1; else return;" |~?= IfElse (LitInt 1) (Return (Just (LitInt 1))) (Return Nothing)
  , "ifelse1" ~: "if (1 2) return 1; else return;" |~?= IfElse ErrorE (Return (Just (LitInt 1))) (Return Nothing)
  , "ifelse1" ~: "if ( 1 ) return 1  ; else\treturn ; " |~?= IfElse (LitInt 1) (Return (Just (LitInt 1))) (Return Nothing)
  , "ifelse1" ~: "if (1) return 1;else return;" |~?= IfElse (LitInt 1) (Return (Just (LitInt 1))) (Return Nothing)
  , "ifelse1" ~: "if (1)\n\treturn 1;\nelse\n\treturn;" |~?= IfElse (LitInt 1) (Return (Just (LitInt 1))) (Return Nothing)
  , "ifelse2" ~: "if (1) if(2) return 1;\nelse\n\treturn;" |~?= If (LitInt 1) (IfElse (LitInt 2) (Return (Just (LitInt 1))) (Return Nothing))
  , "ifelse2" ~: "if (1) if(2) return 1;\nelse\n\treturn; else return 3;" |~?=
                  IfElse (LitInt 1)
                         (IfElse (LitInt 2)
                                 (Return (Just (LitInt 1)))
                                 (Return Nothing))
                         (Return (Just (LitInt 3)))
  , "assign1" ~: "a = 1 ;" |~?= Assign (Assignment (Scalar "a") (LitInt 1))
  , "assign2" ~: "a [ 1 + 2 ] = f(x\t)\n;" |~?= Assign (Assignment (Array "a" (Binary Plus (LitInt 1) (LitInt 2))) (FunctionCall (Function "f" [Var (Scalar "x")])))
  , "for1" ~: "for(i = 0; i< 10; i = i + 1)\n\treturn i;" |~?= For (Just (Assignment (Scalar "i") (LitInt 0)))
                                                                   (Just (Relative Less (Var (Scalar "i")) (LitInt 10)))
                                                                   (Just (Assignment (Scalar "i") (Binary Plus (Var (Scalar "i")) (LitInt 1))))
                                                                   (Return (Just (Var (Scalar "i"))))
  , "for2" ~: "for(; i< 10; i = i + 1)\n\treturn i;" |~?= For Nothing
                                                                  (Just (Relative Less (Var (Scalar "i")) (LitInt 10)))
                                                                  (Just (Assignment (Scalar "i") (Binary Plus (Var (Scalar "i")) (LitInt 1))))
                                                              (Return (Just (Var (Scalar "i"))))
  , "for3" ~: "for(i = 0; ; i = i + 1)\n\treturn i;" |~?= For (Just (Assignment (Scalar "i") (LitInt 0)))
                                                                  Nothing
                                                                  (Just (Assignment (Scalar "i") (Binary Plus (Var (Scalar "i")) (LitInt 1))))
                                                              (Return (Just (Var (Scalar "i"))))
  , "for4" ~: "for(;;)\n\treturn 1-1;" |~?= For Nothing Nothing Nothing
                                                (Return (Just (Binary Minus (LitInt 1) (LitInt 1))))
  , "bracketed1" ~: "if (a == 1) {\n\treturn 1;}\nelse{\n\treturn 0;}" |~?= IfElse (Relative Eq (Var (Scalar "a")) (LitInt 1))
                                                                                   (Bracketed [Return (Just (LitInt 1))])
                                                                                   (Bracketed [Return (Just (LitInt 0))])
  , "bracketed2" ~: "if (a == 1) {}\nelse{\n\treturn 0;}" |~?= IfElse (Relative Eq (Var (Scalar "a")) (LitInt 1))
                                                                                   (Bracketed [])
                                                                                   (Bracketed [Return (Just (LitInt 0))])
  , "bracketed3" ~: bad "if (a == 1) \nelse{\n\treturn 0;}"
  , "assign1" ~: "a = 1;" |~?= Assign (Assignment (Scalar "a") (LitInt 1))
  , "assign1" ~: " a = 1 ; " |~?= Assign (Assignment (Scalar "a") (LitInt 1))
  , "assign1" ~: " a = f(1) ; " |~?= Assign (Assignment (Scalar "a") (FunctionCall (Function "f" [LitInt 1])))
  , "assign1" ~: " a[2] = f(1) ; " |~?= Assign (Assignment (Array "a" (LitInt 2)) (FunctionCall (Function "f" [LitInt 1])))
  , "pc" ~: "f(1);" |~?= ProcedureCall (Function "f" [LitInt 1])
  , "none" ~: " ; " |~?= None
  ]
