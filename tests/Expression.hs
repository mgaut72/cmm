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
                 , TestLabel "testNegative7" testNegative6
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
                 , TestLabel "tChar1" tChar1
                 , TestLabel "tChar2" tChar2
                 , TestLabel "tChar3" tChar3
                 , TestLabel "tChar4" tChar4
                 , TestLabel "tChar5" tChar5
                 , TestLabel "tChar6" tChar6
                 , TestLabel "tStr1" tStr1
                 , TestLabel "tStr2" tStr2
                 , TestLabel "tStr3" tStr3
                 , TestLabel "tMath1" tMath1
                 , TestLabel "tMath2" tMath2
                 , TestLabel "tMath3" tMath3
                 , TestLabel "tMath4" tMath4
                 , TestLabel "tMath5" tMath5
                 , TestLabel "tMath6" tMath6
                 , TestLabel "tMath7" tMath7
                 ]

a |~?= b = readExpr a ~?= Right b
bad a = readExpr a ~?= Left "error"

tChar1 = "\'a\'" |~?= LitChar 'a'
tChar2 = "\'\\n\'" |~?= LitChar '\n'
tChar3 = "\'\\0\'" |~?= LitChar '\0'
tChar4 = bad "\'a"
tChar5 = bad "a"
tChar6 = bad "\'ab\'"

tStr1 = "\"abc\"" |~?= LitString "abc"
tStr2 = bad "\"abc"
tStr3 = "\"a\\nbc\"" |~?= LitString "a\nbc"

testNegative1 = "-1" |~?= Negative (LitInt 1)
testNegative2 = "-'a'" |~?= Right (Negative (LitChar 'a'))
testNegative3 = bad "--2"
testNegative4 = "-\"ab\"" |~?= Negative (LitString "ab")
testNegative5 = "-(-56)" |~?= Negative (Negative (LitInt 56))
testNegative6 = bad "-'56"
testNegative7 = bad "- 56"

testNot1 = "!1" |~?= Not (LitInt 1)
testNot2 = "!'a'" |~?= Not (LitChar 'a')
testNot3 = bad "!-2"
testNot4 = "!\"ab\"" |~?= Not (LitString "ab")
testNot5 = "!(!56)" |~?= Not (Not (LitInt 56))
testNot6 = bad "!'56"

testNotNeg1 = bad "!-1"
testNotNeg2 = bad "-!1"
testNotNeg3 = "-(!1)" |~?= Negative (Not (LitInt 1))
testNotNeg4 = "!(-1)" |~?= Not (Negative (LitInt 1))

tMath1 = "1 + 2" |~?= Binary Plus (LitInt 1) (LitInt 2)
tMath2 = "23 * 1 |+ 2"?= Right (Binary Plus (Binary Times (LitInt 23) (LitInt 1)) (LitInt 2))
tMath3 = "2 * (1 + 24)" |~?= Binary Times (LitInt 2) (Binary Plus (LitInt 1) (LitInt 24))
tMath4 = "2 / 1 * 2" |~?= Binary Times (Binary Divide (LitInt 2) (LitInt 1)) (LitInt 2)
tMath5 = "2 * 1 / 2" |~?= Binary Divide (Binary Times (LitInt 2) (LitInt 1)) (LitInt 2)
tMath6 = "-1 + 2" |~?= Binary Plus (Negative (LitInt 1)) (LitInt 2)
tMath7 = "-(1 + 2)" |~?= Negative (Binary Plus (LitInt 1) (LitInt 2))
