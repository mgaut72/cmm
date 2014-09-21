module Main where
import System.IO
import Language.CMM.Compiler

main = do
  program <- getContents           -- read from stdin
  let (res, errors) = compileCMM program
  hPutStr stderr $ unlines errors
  case res of
       Right a -> putStrLn $ show a
       Left  a -> hPutStrLn stderr $ show a
