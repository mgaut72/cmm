module Main where

import Language.CMM.Compiler

main = do
  program <- getContents           -- read from stdin
  let (res, errors) = compileCMM program
  putStr $ unlines errors
  case res of
       Right a -> putStrLn $ show a
       Left  a -> putStrLn $ show a
