module Main where

import Language.CMM.Compiler

main = do
  program <- getContents           -- read from stdin
  case compileCMM program of
       Right a -> putStrLn $ show a
       Left  a -> putStrLn $ show a
