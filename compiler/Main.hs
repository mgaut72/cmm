module Main where

import Language.CMM.Syntax.Parser

main = do
  program <- getContents           -- read from stdin
  case parseCMM program of
       Right a -> putStrLn $ ("matched : " ++ show a)
       Left  a -> putStrLn $ show a
