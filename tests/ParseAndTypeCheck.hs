import System.Exit
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M
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

initialState :: Tables
initialState = Tables { _globalSymbols = M.fromList symbolList
                      , _localSymbols = M.empty
                      , _functions = M.fromList fcnList
                      , _currentFunctionType = TVoid
                      }

symbolList = [ ("a", TInt), ("b", TChar), ("none", TInt)
             , ("one", TInt), ("two", TInt), ("aa", TArray TInt)
             , ("bb", TArray TChar), ("i", TInt)
             ]
fcnList = [("none", []), ("one", [TInt]), ("two", [TInt, TChar])]


readS a = runWriter . runParserT p initialTables "" $ a
 where p = do
         whiteSpace
         prog <- programP
         eof
         return prog

a |~?= b = readS a ~?= (Right b, [])

bad a = TestCase (when (null errs) (assertFailure ("expected bad parse\ngot: " ++ show res)))
 where (res, errs) = readS a
       isLeft (Right a) = False
       isLeft (Left a) = True

good a = TestCase (unless (null errs) (assertFailure ("expected good parse\ngot: " ++ show (res,errs))))
 where (res, errs) = readS a
       isRight (Right a) = True
       isRight (Left a) = False

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
returnBad = bad "int main(void) { return \"asdf\"; }"
-- returnBad2 = good $ "int main(void) { return 'c'; }" -- currently not sure
-- how this needs to be handled

prototypeAfterDef = bad "int main(void) {return 1;} int main(void);"

