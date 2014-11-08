module Main where

import System.IO
import System.Exit
import Data.List

import Language.CMM.Compiler

main = do
  program <- getContents
  case compileCMM program of
       Right a -> putStrLn a
       Left er -> hPutStrLn stderr er >> exitFailure
