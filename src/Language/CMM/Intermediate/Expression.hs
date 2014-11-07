module Language.CMM.Intermediate.Expression where

import Control.Lens
import Control.Monad

import Data.Monoid
import Data.Maybe
import qualified Data.Map as M

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions

-- returns the ( temp identifier that the expression result is in
--             , the three address code necessary to get there
--             )
genE :: Expression -> TACGen (Identifier, [ThreeAddress])

genE (Negative e) = do
  (iE, tacE) <- genE e >>= convertTo TInt
  tmp <- getTmp >>= recordIdentifier TInt
  return (tmp, tacE <> [AssignMinus tmp (IVar iE)])

-- I feel bad about this, storing a literal into a variable, but it
-- generalizes better.
-- If I want to keep doing this, I can make all operations except assigns
-- and copies work only on IVars
genE (LitInt i) = do
  tmp <- getTmp >>= recordIdentifier TInt
  return (tmp, [Copy tmp (IConst i)])

genE (LitChar c) = do
  tmp <- getTmp >>= recordIdentifier TChar
  return (tmp, [Copy tmp (CConst c)])

-- TODO: something about a pointer bla bla
genE (LitString s) = undefined

genE (Binary op e1 e2) = do
  (iE1, tacE1) <- genE e1 >>= convertTo TInt
  (iE2, tacE2) <- genE e2 >>= convertTo TInt
  tmp <- getTmp >>= recordIdentifier TInt
  return (tmp, tacE1 <> tacE2 <> [AssignBinary tmp op (IVar iE1) (IVar iE2)])

genE (FunctionCall (Function i es)) = do




-- Booleans are special cases where we jump all over the place, so they get
-- their own generator functions

genBooleanE :: Expression -> TACGen (LabelName, LabelName, [ThreeAddress])

genBooleanE (Relative op e1 e2) = do
  (iE1, tacE1) <- genE e1 >>= convertTo TInt
  (iE2, tacE2) <- genE e2 >>= convertTo TInt
  trueL <- getLabel
  falseL <- getLabel
  let newCode = [IIf iE1 op iE2 trueL falseL]
  return (trueL, falseL, tacE1 <> tacE2 <> newCode)

genBooleanE (Logical Or e1 e2) = do
  (tL1, fL1, tacE1) <- genBooleanE e1
  (tL2, fL2, tacE2) <- genBooleanE e2
  trueL <- getLabel
  falseL <- getLabel
  let true1 = [Label tL1, GoTo trueL] -- short circuit
  let false1 = Label fL1 : tacE2      -- fall through to check second one
  let true2 = [Label tL2, GoTo trueL]
  let false2 = [Label fL2, GoTo falseL]
  let newCode = tacE1 <> true1 <> false1 <> true2 <> false2
  return (trueL, falseL, newCode)

genBooleanE (Logical And e1 e2) = do
  (tL1, fL1, tacE1) <- genBooleanE e1
  (tL2, fL2, tacE2) <- genBooleanE e2
  trueL <- getLabel
  falseL <- getLabel
  let false1 = [Label fL1, GoTo falseL] -- short circuit
  let true1 = Label tL1 : tacE2         -- case: have to eval the 2nd one
  let true2 = [Label tL2, GoTo trueL]
  let false2 = [Label fL2, GoTo falseL]
  let newCode = tacE1 <> false1 <> true1 <> true2 <> false2
  return (trueL, falseL, newCode)

genBooleanE (Not e) = do
  (tL, fL, tacE) <- genBooleanE e
  trueL <- getLabel
  falseL <- getLabel
  return (trueL, falseL, tacE <> [Label tL, GoTo falseL, Label fL, GoTo trueL])
