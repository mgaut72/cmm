module Language.CMM.TypeChecker.Function where

import Language.CMM.AST

typeOfFunction :: Bool -> Function -> MyParser TType
