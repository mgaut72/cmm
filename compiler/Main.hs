module Main where

import System.IO
import Data.List

import Language.CMM.Compiler

main = do
  program <- getContents
  case compileCMM program of
       (Right a, []) -> return ()
       (Right a, es) -> hPutStr stderr $ errorMessages es
       (Left  a, es) -> hPutStrLn stderr $ errorMessages es ++ show a
 where errorMessages = unlines . nub
