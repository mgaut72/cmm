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
                 , TestLabel "testNot1" testNot1
                 , TestLabel "testNot2" testNot2
                 , TestLabel "testNot3" testNot3
                 , TestLabel "testNot4" testNot4
                 , TestLabel "testNot5" testNot5
                 , TestLabel "testNot6" testNot6
                 , TestLabel "testNotNeg1" testNotNeg1
                 , TestLabel "testNotNeg2" testNotNeg2
                 , TestLabel "testNotNeg3" testNotNeg3
                 , TestLabel "testNotNeg4" testNotNeg4
                 ]



testNegative1 = readExpr "-1" ~=? Right (Negative (LitInt 1))
testNegative2 = readExpr "-'a'" ~=? Right (Negative (LitChar 'a'))
testNegative3 = readExpr "--2" ~=? Left "error"
testNegative4 = readExpr "-\"ab\"" ~=? Right (Negative (LitString "ab"))
testNegative5 = readExpr "-(-56)" ~=? Right (Negative (Negative (LitInt 56)))
testNegative6 = readExpr "-'56" ~=? Left "error"

testNot1 = readExpr "!1" ~=? Right (Not (LitInt 1))
testNot2 = readExpr "!'a'" ~=? Right (Not (LitChar 'a'))
testNot3 = readExpr "!-2" ~=? Left "error"
testNot4 = readExpr "!\"ab\"" ~=? Right (Not (LitString "ab"))
testNot5 = readExpr "!(!56)" ~=? Right (Not (Not (LitInt 56)))
testNot6 = readExpr "!'56" ~=? Left "error"

testNotNeg1 = readExpr "!-1" ~=? Left "error"
testNotNeg2 = readExpr "-!1" ~=? Left "error"
testNotNeg3 = readExpr "-(!1)" ~=? Right (Negative (Not (LitInt 1)))
testNotNeg4 = readExpr "!(-1)" ~=? Right (Not (Negative (LitInt 1)))
