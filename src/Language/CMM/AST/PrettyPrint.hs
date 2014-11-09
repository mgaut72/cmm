module Language.CMM.AST.PrettyPrint where

import Data.List

import Language.CMM.AST

class Pretty a where
  pretty :: a -> String

instance Pretty Program where
  pretty (Program ps) = concatMap pretty ps

instance Pretty ProgData where
  pretty (Decl d) = pretty d
  pretty (Func f) = pretty f

instance Pretty Declaration where
  pretty (VariableDecl v) = pretty v
  pretty (FunctionDecl b t fs)
    | b     = "extern " ++ fd
    | not b = fd
   where fd = pretty t ++ " " ++ (intercalate ", " . map pretty $ fs) ++ ";\n"

instance Pretty FuncStub where
  pretty (FuncStub i p) = i ++ "(" ++ pretty p ++ ")"

instance Pretty Expression where
  pretty (Negative e)       = "-" ++ pretty e
  pretty (Not e)            = "!" ++ pretty e
  pretty (LitInt i)         = show i
  pretty (LitChar c)        = show c
  pretty (LitString s)      = show s
  pretty (Binary o e1 e2)   = "(" ++ pretty e1 ++ " " ++ pretty o ++ " " ++ pretty e2 ++ ")"
  pretty (Relative o e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty o ++ " " ++ pretty e2 ++ ")"
  pretty (Logical o e1 e2)  = "(" ++ pretty e1 ++ " " ++ pretty o ++ " " ++ pretty e2 ++ ")"
  pretty (FunctionCall f)   = pretty f
  pretty (Var v)            = pretty v
  pretty ErrorE             = "<ERROR>"

instance Pretty FunctionDef where
  pretty (FunctionDef t i ps vds ss) = heading ++ decls ++ "\n" ++ stmts ++ "}\n"
   where heading = pretty t ++ " " ++ i ++ "(" ++ pretty ps ++ "){\n"
         decls = indent $ concatMap pretty vds
         stmts = indent $ concatMap pretty ss

instance Pretty Parameters where
  pretty VoidParameter   = "void"
  pretty (Parameters ps) = intercalate ", " . map pretty $ ps

instance Pretty Parameter where
  pretty (ScalarParam t i) = pretty t ++ " " ++ i
  pretty (ArrayParam  t i) = pretty t ++ "[] " ++ i

instance Pretty VarDecl where
  pretty (VarDecl t vs) = pretty t ++ " " ++ (intercalate ", " . map pretty $ vs) ++ ";\n"

instance Pretty Statement where
  pretty (If e s@(Bracketed _)) = "if(" ++ pretty e ++ ")" ++ pretty s
  pretty (If e s) = "if(" ++ pretty e ++ ")\n" ++ indent (pretty s)
  pretty (IfElse e s1 s@(Bracketed _)) = pretty (If e s1) ++ "else" ++ pretty s
  pretty (IfElse e s1 s) = pretty (If e s1) ++ "else\n" ++ indent (pretty s)
  pretty (While e s@(Bracketed _)) = "while(" ++ pretty e ++ ")" ++ pretty s
  pretty (While e s) = "while(" ++ pretty e ++ ")\n" ++ indent (pretty s)
  pretty (For ma1 me ma2 s@(Bracketed _)) = "for(" ++ prettyM ma1 ++ "; "
                                        ++ prettyM me ++ "; " ++ prettyM ma2 ++ ")"
                                        ++ pretty s
  pretty (For ma1 me ma2 s) = "for(" ++ prettyM ma1 ++ "; "
                                   ++ prettyM me ++ "; " ++ prettyM ma2 ++ ")"
                                   ++ indent (pretty s)
  pretty (Return me) = "return " ++ prettyM me ++ ";" ++ "\n"
  pretty (Assign a) = pretty a ++ ";" ++ "\n"
  pretty (Bracketed ss) = "{\n" ++ indent (concatMap pretty ss) ++ "}" ++ "\n"
  pretty None = ";" ++ "\n"
  pretty (ProcedureCall f) = pretty f ++ ";" ++ "\n"
  pretty ErrorS = "<ERROR>"

prettyM Nothing = ""
prettyM (Just a) = pretty a

indent a = unlines . map (\s -> "    " ++ s) . lines $ a

instance Pretty Variable where
  pretty (Scalar i) = i
  pretty (Array i e) = i ++ "[" ++ pretty e ++ "]"

instance Pretty Function where
  pretty (Function i es) = i ++ "(" ++ (intercalate ", " . map pretty $ es) ++ ")"

instance Pretty Assignment where
  pretty (Assignment v e) = pretty v ++ " = " ++ pretty e
  pretty ErrorA = "<ERROR>"

instance Pretty LogicalOp where
  pretty And = "&&"
  pretty Or  = "||"

instance Pretty RelativeOp where
  pretty Eq = "=="
  pretty Neq = "!="
  pretty Leq = "<="
  pretty Geq = ">="
  pretty Less = "<"
  pretty Greater = ">"

instance Pretty BinaryOp where
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Times = "*"
  pretty Divide = "/"

instance Pretty TType where
  pretty (TArray t _) = pretty t ++ "[]"
  pretty TBool = "bool"
  pretty TInt  = "int"
  pretty TChar = "char"
  pretty TVoid = "void"
  pretty TError = "<ERROR>"

