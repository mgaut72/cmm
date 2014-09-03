import System.Exit
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Language.CMM.Syntax.AST
import Language.CMM.Syntax.Parser

main = do
  cs <- runTestTT tests
  if (failures cs) /= 0 || (errors cs) /= 0
     then exitFailure
     else exitSuccess

readExpr s = case parse (whiteSpace >> expressionP) "foo" s of
                  Right a -> Right a
                  Left b -> Left "error"


tests = TestList [ TestLabel "testNegative1" testNegative1
                 , TestLabel "testNegative2" testNegative2
                 , TestLabel "testNegative3" testNegative3
                 , TestLabel "testNegative4" testNegative4
                 , TestLabel "testNegative5" testNegative5
                 , TestLabel "testNegative6" testNegative6
                 ]



testNegative1 = readExpr "-1" ~=? Right (Negative (LitInt 1))
testNegative2 = readExpr "-'a'" ~=? Right (Negative (LitChar 'a'))
testNegative3 = readExpr "--2" ~=? Left "error"
testNegative4 = readExpr "-\"ab\"" ~=? Right (Negative (LitString "ab"))
testNegative5 = readExpr "-(-56)" ~=? Right (Negative (Negative (LitInt 56)))
testNegative6 = readExpr "-'56" ~=? Left "error"

