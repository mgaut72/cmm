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

genS (If e s) = do
  (t,f,code) <- genBooleanE e
  sCode <- genS s
  return $ code <> [Label t] <> sCode <> [Label f]

genS (IfElse e s1 s2) = do
  (t,f,eCode) <- genBooleanE e
  s1Code <- genS s1
  s2Code <- genS s2
  end <- getLabel
  return $ eCode <> [Label t] <> s1Code <> [GoTo end] <> [Label f] <> s2Code <> [Label end]


genS (While e s) = do
  (t,f,eCode) <- genBooleanE e
  sCode <- genS s
  start <- getLabel
  return $ [Label start] <> eCode <> [Label t] <> sCode <> [GoTo start] <> [Label f]


genS (For ma1 me ma2 s) = do
  asgn1 <- genS $ maybeAssign ma1
  let asgn2 = maybeAssign ma2
  let newS = case s of
               Bracketed ss -> Bracketed (ss <> [asgn2])
               _            -> Bracketed [s, asgn2]
  while <- genS (While e newS)
  return $ asgn1 <> while
 where maybeAssign ma = case ma of
         Just a  -> Assign a
         Nothing -> None
       e = case me of
         Just e  -> e
         Nothing -> Relative Eq (LitInt 1) (LitInt 1) -- some default true value



genS (Return Nothing) = do
  l <- leave
  return $ l <> pure (Ret Nothing)

genS (Return (Just e)) = do
  f <- use currFcn
  retType <- lookupSymb f
  (tE, codeE) <- genE e >>= convertTo retType
  return $ codeE <> pure (Ret (Just tE))

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

genS (Bracketed ss) = liftM concat . mapM genS $ ss

genS e = error $ show e ++ " is not supported"

leave = liftM (pure . Leave) (use currFcn)
