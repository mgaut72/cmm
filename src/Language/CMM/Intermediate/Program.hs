module Language.CMM.Intermediate.Program where

import Control.Monad.State

import Data.Maybe

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions
import Language.CMM.Intermediate.FunctionDef

genP :: (Program, Tables) -> [([ThreeAddress], Symbols)]
genP (Program ps, t) = map fromJust 
                        . filter isJust 
                        . map (genPData t) $ ps


genPData _ (Decl _) = Nothing

genPData t (Func f@(FunctionDef _ i _ _ _ )) = Just $ runState (genF f) s
 where s = tablesToSymbols t i
