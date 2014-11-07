module Language.CMM.Intermediate.Statement where

import Control.Applicative
import Control.Lens
import Control.Monad

import Data.Monoid

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions
import Language.CMM.Intermediate.FunctionCall
import Language.CMM.Intermediate.Expression

genS :: Statement -> TACGen [ThreeAddress]

genS (If e s) = undefined

genS (IfElse e s1 s2) = undefined

genS (While e s) = undefined

genS (For ma1 me ma2 s) = undefined

genS x@(Return Nothing) = undefined

genS (Return (Just e)) = undefined

genS (ProcedureCall f) = genFCall f

genS None = undefined

genS (Assign (Assignment (Scalar i) e)) = do
  varType <- lookupSymb i
  (iE, tacE) <- genE e >>= convertTo varType
  return $ tacE <> pure (Copy i (IVar iE))

genS (Assign (Assignment (Array i idx) e)) = undefined

genS (Bracketed ss) = undefined
