import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Error

import Language.CMM.AST
import Language.CMM.Parser.Typed
import Language.CMM.Parser.Base
import Language.CMM.Compiler

main = do
  cs <- runTestTT tests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess

readS a = runWriter . runParserT p initialTables "" $ a
 where p = do
         whiteSpace
         prog <- programP
         eof
         return prog

a |~?= b = readS a ~?= (Right b, [])

bad a = TestCase (when (isGood a) (assertFailure ("expected bad parse\ngot: " ++ show (readS a))))

isGood a = case readS a of
            (Right _, []) -> True
            otherwise     -> False

good a = TestCase (unless (isGood a) (assertFailure ("expected good parse\ngot: " ++ show (readS a))))

instance (Eq ParseError) where
  a == b = errorMessages a == errorMessages b

tests = TestList [ TestLabel "goodArraySize" goodArraySize
                 , TestLabel "zeroArraySize" zeroArraySize
                 , TestLabel "negativeArraySize" negativeArraySize
                 , TestLabel "globalIdentifier" globalIdentifier
                 , TestLabel "localIdentifier" localIdentifier
                 , TestLabel "bothIdentifier" bothIdentifier
                 , TestLabel "doubleGlobal" doubleGlobal
                 , TestLabel "doubleGlobal2" doubleGlobal2
                 , TestLabel "doubleLocal" doubleLocal
                 , TestLabel "doubleLocal2" doubleLocal2
                 , TestLabel "noPrototype" noPrototype
                 , TestLabel "onePrototype" onePrototype
                 , TestLabel "twoPrototype" twoPrototype
                 , TestLabel "twoPrototype2" twoPrototype2
                 , TestLabel "prototypeMatch" prototypeMatch
                 , TestLabel "prototypeReturnNoMatch" prototypeReturnNoMatch
                 , TestLabel "prototypeReturnNoMatch2" prototypeReturnNoMatch2
                 , TestLabel "prototypeParamNoMatch" prototypeParamNoMatch
                 , TestLabel "returnGood" returnGood
                 , TestLabel "returnBad" returnBad
                 , TestLabel "prototypeAfterDef" prototypeAfterDef
                 , TestLabel "prototypeAfterDef2" prototypeAfterDef2
                 , TestLabel "identifierTwiceInPrototype" identifierTwiceInPrototype
                 , TestLabel "identifierTwiceInDef" identifierTwiceInDef
                 , TestLabel "badNoParamters" badNoParamters
                 , TestLabel "goodNoParamters" goodNoParamters
                 , TestLabel "goodExternFunction" goodExternFunction
                 , TestLabel "badExternFunction" badExternFunction
                 ]


goodArraySize = good $ unlines
  [ "int arr1[10];" -- postitive ok
  ]
zeroArraySize = bad "int arr[0];"
negativeArraySize = bad "int arr[-1];"

globalIdentifier = good "int var;"
localIdentifier = good "void main(void){ int var; }"
bothIdentifier  = good $ unlines [ "int var;"
                                  , "void main(void){"
                                  , "int var;"
                                  , "}"
                                  ]

doubleGlobal = bad "int var, var;"
doubleGlobal2 = bad "int var; int var;"

doubleLocal = bad "void main(void){ int var, var; }"
doubleLocal2 = bad "void main(void){ int var; int var; }"

noPrototype = good "void main(void) { return; }"
onePrototype = good "int main(int a); int main(int b){ return 1; }"
twoPrototype = bad "int main(int a); int main(int b); int main(int c){ return 1; }"
twoPrototype2 = bad "int main(int a); int main(int a); int main(int b){ return 1; }"

prototypeMatch = good "int main(int a); int main(int b){ return 1; }"
prototypeReturnNoMatch = bad "int main(int a); void main(int b){ return; }"
prototypeReturnNoMatch2 = bad "int main(int a); char main(int b){ return; }"
prototypeParamNoMatch = bad "int main(int a); int main(char b){ return 1; }"

returnGood = good "int main(void) { return 1; }"
returnGood2 = good "int main(void) { return 'c'; }" -- conversion is allowed
returnBad = bad "int main(void) { return \"asdf\"; }"

prototypeAfterDef = bad "int main(void) {return 1;} int main(void);"
prototypeAfterDef2 = bad "int main(void) {return 1;}\nint main(void);"

identifierTwiceInPrototype = bad "int main(int a, int a);"
identifierTwiceInDef = bad "int main(int a, int a) {return 1;}"

badNoParamters = bad "int main(){return 1;}"
goodNoParamters = good "int main(void){return 1;}"

badExternFunction = bad "extern int foo(void); int foo(void){return 1;}"
goodExternFunction = good "extern int foo(void); int main(void){return 1;}"
