module Language.CMM.Compiler (
  compileCMM
)
where

import Text.Parsec
import Data.List
import Data.Function (on)
import Control.Monad.Writer

import Language.CMM.AST
import Language.CMM.Parser.Typed
import Language.CMM.Parser.Base

import Language.CMM.Intermediate.Program

compileCMM :: String -> Either String String -- error or output
compileCMM srcTxt = case parseCMM srcTxt of
       (Right a, []) -> Right . show $ genP a
       (Right _, es) -> Left es
       (Left  a, es) -> Left $ es ++ show a

parseCMM = makeMessages . runWriter . runParserT p initialTables "compile"
 where p = do
         whiteSpace
         prog <- programP
         eof
         s <- getState
         return (prog, s)

makeMessages (a, w) = (a, msgs)
 where msgs = unlines . map outputToMsg . nubBy ((==) `on` snd) $ w
       outputToMsg (p, m) = "Error near line " ++ show (sourceLine p) ++
                            ", column " ++ show (sourceColumn p) ++
                            ":\n\t" ++ m ++ "\n\n"
