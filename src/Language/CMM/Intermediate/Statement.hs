module Language.CMM.Intermediate.Statement where

import Language.CMM.Intermediate.Function

genS :: Statement -> TACGen [ThreeAddress]

genS x@(If e s) = undefined

genS x@(IfElse e s1 s2) = undefined

genS x@(While e s) = undefined

genS x@(For ma1 me ma2 s) = undefined

genS x@(Return Nothing) = undefined

genS x@(Return (Just e)) = undefined

genS (ProcedureCall f) = genF f

genS None = undefined

genS x@(Assign (Assignment (Scalar i) e)) = do
  varType <- lookupSymb i
  (iE, tacE) <- genE e >>= convertTo varType
  return $ tacE <> [Copy i (IVar iE)]

genS x@(Assign (Assignment (Array i idx) e)) = undefined

genS x@(Bracketed ss) = undefined
