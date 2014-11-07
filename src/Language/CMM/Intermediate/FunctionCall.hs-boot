module Language.CMM.Intermediate.FunctionCall where

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions

genFCall :: Function -> TACGen [ThreeAddress]
