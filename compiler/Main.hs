module Main where

import Language.CMM.Compiler

main = do
  program <- getContents           -- read from stdin
  res <- compileCMM program
  case res of
       Right a -> putStrLn $ show a
       Left  a -> putStrLn $ show a
