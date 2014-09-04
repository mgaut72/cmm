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
  ]
