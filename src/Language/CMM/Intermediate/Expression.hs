module Language.CMM.Intermediate.Expression where

import Control.Lens
import Control.Monad

import Data.Monoid
import qualified Data.Map as M

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions

-- returns the ( temp identifier that the expression result is in
--             , the three address code necessary to get there
--             )
genE :: Expression -> TACGen (Identifier, [ThreeAddress])

genE (Negative e) = do
  (iE, tacE) <- genE e
  tmp <- getTmp
  return (tmp, tacE <> [AssignMinus tmp (IVar iE)])

-- assumes e was appropriately generated as a boolean type
genE (Not e) = do
  (iE, tacE) <- genE e
  tmp <- getTmp
  return (tmp, tacE <> [AssignNot tmp (IVar iE)])

-- I feel bad about this, storing a literal into a variable, but it
-- generalizes better.
-- If I want to keep doing this, I can make all operations except assigns
-- and copies work only on IVars
genE (LitInt i) = do
  tmp <- getTmp
  return (tmp, [Copy tmp (IConst i)])

genE (LitChar c) = do
  tmp <- getTmp
  return (tmp, [Copy tmp (CConst c)])

-- TODO: something about a pointer bla bla
genE (LitString s) = undefined

genE (Binary op e1 e2) = do
  (iE1, tacE1) <- genE e1
  (iE2, tacE2) <- genE e2
  -- TODO: check to see if we need to convert the types
  tmp <- getTmp
  return (tmp, tacE1 <> tacE2 <> [AssignBinary tmp Plus (IVar iE1) (IVar iE2)])

genE (Relative op e1 e2) = do
  (iE1, tacE1) <- genE e1
  (iE2, tacE2) <- genE e2
  -- TODO: check to see if we need to convert the types
  t <- getTmp
  code <- case op of
    Eq  -> return [AssignBinary t Plus (IVar iE1) (IVar iE2)]
    Neq -> do
      (i, tac) <- genE (Relative Eq e1 e2)
      return $ tac <> [AssignNot t (IVar i)]
  return (t, tacE1 <> tacE2 <> code)
