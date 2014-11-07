module Language.CMM.Intermediate.Function where

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions

genF :: Function -> TACGen [ThreeAddress]
