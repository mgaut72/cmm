{-# LANGUAGE TemplateHaskell #-}
module Language.CMM.AST where

import qualified Data.Map as M
import Data.List
import Control.Lens
import Control.Monad.Writer
import Text.Parsec.Prim (ParsecT)

-- Parser type
type MyParser a = ParsecT String Tables (Writer [String]) a


-- Language tree types

data Program = Program [ProgData] deriving (Eq)

instance Show Program where
  show (Program ps) = concatMap show ps

data ProgData = Decl Declaration
              | Func FunctionDef
              deriving (Eq)

instance Show ProgData where
  show (Decl d) = show d
  show (Func f) = show f

data Declaration = VariableDecl     VarDecl
                 | FunctionDecl     IsExtern TType [FuncStub]
                 deriving (Eq)

instance Show Declaration where
  show (VariableDecl v) = show v
  show (FunctionDecl b t fs)
    | b     = "extern " ++ fd
    | not b = fd
   where fd = show t ++ " " ++ (intercalate ", " . map show $ fs) ++ ";\n"

data FuncStub = FuncStub Identifier Parameters deriving (Eq)

instance Show FuncStub where
  show (FuncStub i p) = i ++ "(" ++ show p ++ ")"

type IsExtern = Bool

data Expression = Negative      Expression
                | Not           Expression
                | LitInt        Integer
                | LitChar       Char
                | LitString     String
                | Binary        BinaryOp Expression Expression
                | Relative      RelativeOp Expression Expression
                | Logical       LogicalOp Expression Expression
                | FunctionCall  Function
                | Var           Variable
                | ErrorE
                deriving (Eq)

instance Show Expression where
  show (Negative e)       = "-" ++ show e
  show (Not e)            = "!" ++ show e
  show (LitInt i)         = show i
  show (LitChar c)        = show c
  show (LitString s)      = show s
  show (Binary o e1 e2)   = "(" ++ show e1 ++ " " ++ show o ++ " " ++ show e2 ++ ")"
  show (Relative o e1 e2) = "(" ++ show e1 ++ " " ++ show o ++ " " ++ show e2 ++ ")"
  show (Logical o e1 e2)  = "(" ++ show e1 ++ " " ++ show o ++ " " ++ show e2 ++ ")"
  show (FunctionCall f)   = show f
  show (Var v)            = show v
  show ErrorE             = "<ERROR>"


data FunctionDef = FunctionDef TType Identifier Parameters [VarDecl] [Statement]
                 deriving (Eq)

instance Show FunctionDef where
  show (FunctionDef t i ps vds ss) = heading ++ decls ++ "\n" ++ stmts ++ "}\n"
   where heading = show t ++ " " ++ i ++ "(" ++ show ps ++ "){\n"
         decls = indent $ concatMap show vds
         stmts = indent $ concatMap show ss

data Parameters = VoidParameter
                | Parameters [Parameter]
                deriving (Eq)

instance Show Parameters where
  show VoidParameter   = "void"
  show (Parameters ps) = intercalate ", " . map show $ ps

data Parameter = ScalarParam TType Identifier
               | ArrayParam  TType Identifier
               deriving (Eq)

instance Show Parameter where
  show (ScalarParam t i) = show t ++ " " ++ i
  show (ArrayParam  t i) = show t ++ "[] " ++ i

data VarDecl = VarDecl TType [Variable] deriving (Eq)

instance Show VarDecl where
  show (VarDecl t vs) = show t ++ " " ++ (intercalate ", " . map show $ vs) ++ ";\n"

data Statement = If Expression Statement
               | IfElse Expression Statement Statement
               | While Expression Statement
               | For (Maybe Assignment) (Maybe Expression) (Maybe Assignment) Statement
               | Return (Maybe Expression)
               | Assign Assignment
               | ProcedureCall Function
               | Bracketed [Statement]
               | None
               | ErrorS
               deriving (Eq)

instance Show Statement where
  show (If e s@(Bracketed ss)) = "if(" ++ show e ++ ")" ++ show s
  show (If e s) = "if(" ++ show e ++ ")\n" ++ indent (show s)
  show (IfElse e s1 s@(Bracketed ss)) = show (If e s1) ++ "else" ++ show s
  show (IfElse e s1 s) = show (If e s1) ++ "else\n" ++ indent (show s)
  show (While e s@(Bracketed ss)) = "while(" ++ show e ++ ")" ++ show s
  show (While e s) = "while(" ++ show e ++ ")\n" ++ indent (show s)
  show (For ma1 me ma2 s@(Bracketed ss)) = "for(" ++ showM ma1 ++ "; "
                                        ++ showM me ++ "; " ++ showM ma2 ++ ")"
                                        ++ show s
  show (For ma1 me ma2 s) = "for(" ++ showM ma1 ++ "; "
                                   ++ showM me ++ "; " ++ showM ma2 ++ ")"
                                   ++ indent (show s)
  show (Return me) = "return " ++ showM me ++ ";" ++ "\n"
  show (Assign a) = show a ++ ";" ++ "\n"
  show (Bracketed ss) = "{\n" ++ indent (concatMap show ss) ++ "}" ++ "\n"
  show None = ";" ++ "\n"
  show (ProcedureCall f) = show f ++ ";" ++ "\n"
  show ErrorS = "<ERROR>"

showM Nothing = ""
showM (Just a) = show a

indent a = unlines . map (\s -> "    " ++ s) . lines $ a


data Variable = Scalar Identifier
              | Array Identifier Expression
              deriving (Eq)

instance Show Variable where
  show (Scalar i) = i
  show (Array i e) = i ++ "[" ++ show e ++ "]"

data Function = Function Identifier [Expression] deriving (Eq)

instance Show Function where
  show (Function i es) = i ++ "(" ++ (intercalate ", " . map show $ es) ++ ")"

data Assignment = Assignment Variable Expression
                | ErrorA
                deriving (Eq)

instance Show Assignment where
  show (Assignment v e) = show v ++ " = " ++ show e
  show ErrorA = "<ERROR>"

type Identifier = String

data LogicalOp  = And | Or deriving (Eq)

instance Show LogicalOp where
  show And = "&&"
  show Or  = "||"

data RelativeOp = Eq | Neq | Leq | Less | Geq | Greater deriving (Eq)

instance Show RelativeOp where
  show Eq = "=="
  show Neq = "!="
  show Leq = "<="
  show Geq = ">="
  show Less = "<"
  show Greater = ">"

data BinaryOp = Plus | Minus | Times | Divide  deriving (Eq)

instance Show BinaryOp where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"



-- Other AST related structures


data TType = TBool
           | TChar
           | TInt
           | TVoid
           | TArray TType
           | TError
           deriving (Eq)

compatible TBool TBool = True
compatible TChar TChar = True
compatible TInt  TInt  = True
compatible TVoid TVoid = True
compatible TChar TInt  = True
compatible TInt  TChar = True
compatible (TArray t1) (TArray t2) = compatible t1 t2
compatible _ _ = False

instance Show TType where
  show (TArray t) = show t ++ "[]"
  show TBool = "bool"
  show TInt  = "int"
  show TChar = "char"
  show TVoid = "void"
  show TError = "<ERROR>"


type SymbolTable = M.Map Identifier TType
type FunctionArgumentTable = M.Map Identifier [TType]

data Tables = Tables { _globalSymbols       :: SymbolTable
                     , _localSymbols        :: SymbolTable
                     , _functions           :: FunctionArgumentTable
                     , _currentFunctionType :: TType
                     } deriving (Show, Eq)

makeLenses ''Tables

initialTables = Tables { _globalSymbols       = M.empty
                       , _localSymbols        = M.empty
                       , _functions           = M.empty
                       , _currentFunctionType = TVoid
                       }
