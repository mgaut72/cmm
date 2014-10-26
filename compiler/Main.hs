module Main where

import System.IO
import Data.List

import Language.CMM.Compiler

main = do
  program <- getContents
  case compileCMM program of
       (Right a, []) -> return ()
       (Right a, es) -> hPutStr stderr es
       (Left  a, es) -> hPutStrLn stderr $ es ++ show a
