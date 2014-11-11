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

genS (If e s) = error "if not yet supported"

genS (IfElse e s1 s2) = error "ifelse not yet supported"

genS (While e s) = error "while not yet supported"

genS (For ma1 me ma2 s) = error "for not yet supported"

genS (Return Nothing) = do
  l <- leave
  return $ l <> pure (Ret Nothing)

genS (Return (Just e)) = do
  f <- use currFcn
  retType <- lookupSymb f
  (tE, codeE) <- genE e >>= convertTo retType
  l <- leave
  return $ codeE <> l <> pure (Ret (Just tE))

genS (ProcedureCall f) = genFCall f

genS None = return []

genS (Assign (Assignment (Scalar i) e)) = do
  varType <- lookupSymb i
  (iE, tacE) <- genE e >>= convertTo varType
  return $ tacE <> pure (Copy i (IVar iE))

genS (Assign (Assignment (Array i idx) e)) = do
  varType <- lookupSymb i
  (iE, tacE) <- genE e >>= convertTo varType
  (iIdx, tacIdx) <- genE idx >>= convertTo TInt
  return $ tacE <> tacIdx <> pure (AssignToArr i (IVar iIdx) (IVar iE))

genS (Bracketed ss) = error "bracketed statements not supported"

genS e = error $ show e ++ " is not supported"

leave = liftM (pure . Leave) (use currFcn)
