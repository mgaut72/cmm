module Language.CMM.Intermediate.Statement where

cS :: Statement -> [ThreeAddress]

cS x@(If e s) = undefined

cS x@(IfElse e s1 s2) = undefined

cS x@(While e s) = undefined

cS x@(For ma1 me ma2 s) = undefined

cS x@(Return Nothing) = undefined

cS x@(Return (Just e)) = undefined

cS x@(ProcedureCall f) = undefined

cS None = undefined

cS x@(Assign a) = undefined

cS x@(Bracketed ss) = undefined
