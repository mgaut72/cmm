import System.Exit
import System.Directory
import Test.HUnit
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.List
import Text.Parsec
import Text.Parsec.Error

import Language.CMM.AST
import Language.CMM.Parser.Typed
import Language.CMM.Parser.Base
import Language.CMM.Compiler

-- this gets every examples/*.cmm file and compiles it
main = do
  cmmFiles1 <- getDirectoryContents dir1 >>= filterCMM >>= prependDir dir1
  cmmFiles2 <- getDirectoryContents dir2 >>= filterCMM >>= prependDir dir2
  let cmmFiles = cmmFiles1 ++ cmmFiles2
  cmmContents <- mapM readFile cmmFiles
  cmmTests <- mapM makeTest $ zip cmmFiles cmmContents
  cs <- runTestTT $ test cmmTests
  if failures cs /= 0 || errors cs /= 0
     then exitFailure
     else exitSuccess
 where dir1 = "./cmm-examples/milestone1/"
       dir2 = "./cmm-examples/milestone1/"
       prependDir d fs = return $ map (d ++) fs
       filterCMM fs = return $ filter (isSuffixOf ".c") fs
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
