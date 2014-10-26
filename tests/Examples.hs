import System.Exit
import System.Directory
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import Data.List
import Text.Parsec
import Text.Parsec.Error

import Language.CMM.AST
import Language.CMM.Parser.Typed
import Language.CMM.Parser.Base
import Language.CMM.Compiler

-- this gets every examples/*.cmm file and compiles it
main = do
  cmmFiles <- getDirectoryContents dir >>= filterCMM >>= prependDir dir
  cmmContents <- mapM readFile cmmFiles
  cmmTests <- mapM makeTest $ zip cmmFiles cmmContents
  cs <- runTestTT $ test cmmTests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess
 where dir = "./examples/"
       prependDir d fs = return $ map (d ++) fs
       filterCMM fs = return $ filter (isSuffixOf ".cmm") fs
       makeTest (fname, cs) = return $ fname ~: good cs

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
