module Main where
import System.IO
import Language.CMM.Compiler

main = do
  program <- getContents
  case compileCMM program of
       (Right a, []) -> putStrLn $ show a
       (Right a, es) -> hPutStr stderr $ unlines es
       (Left  a, es) -> hPutStrLn stderr $ unlines es ++ show a
