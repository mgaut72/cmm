{-# LANGUAGE TemplateHaskell #-}
module Language.CMM.Syntax.AST where

import Data.Map.Strict as M
import Control.Lens
import Text.Parsec.Prim (Parsec)

-- Parser type
type MyParser a = Parsec String Tables a


-- Language tree types

data Program = Program [ProgData] deriving (Show, Eq)

data ProgData = Decl Declaration
              | Func FunctionDef
              deriving (Show, Eq)

data Declaration = VariableDecl     VarDecl
                 | FunctionDecl     IsExtern Type [FuncStub]
                 deriving (Show, Eq)

data FuncStub = FuncStub Identifier Parameters deriving (Show, Eq)

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
                deriving (Show, Eq)


data FunctionDef = FunctionDef Type Identifier Parameters [VarDecl] [Statement]
                 deriving (Show, Eq)

data Parameters = VoidParameter
                | Parameters [Parameter]
                deriving (Show, Eq)

data Parameter = ScalarParam Type Identifier
               | ArrayParam  Type Identifier
               deriving (Show, Eq)

data VarDecl = VarDecl Type [Variable] deriving (Show, Eq)

data Type = Char | Int | Void deriving (Show, Eq)

data Statement = If Expression Statement
               | IfElse Expression Statement Statement
               | While Expression Statement
               | For (Maybe Assignment) (Maybe Expression) (Maybe Assignment) Statement
               | Return (Maybe Expression)
               | Assign Assignment
               | ProcedureCall Function
               | Bracketed [Statement]
               | None
               deriving (Show, Eq)

data Variable = Scalar Identifier
              | Array Identifier Expression
              deriving (Show, Eq)

data Function = Function Identifier [Expression] deriving (Show, Eq)

data Assignment = Assignment Variable Expression
                deriving (Show, Eq)

type Identifier = String

data LogicalOp  = And | Or deriving (Show, Eq)
data RelativeOp = Eq | Neq | Leq | Less | Geq | Greater deriving (Show, Eq)
data BinaryOp   = Plus | Minus | Times | Divide  deriving (Show, Eq)


-- Other AST related structures


data TType = TBool
           | TChar
           | TInt
           | TVoid
           | TArray TType
           deriving (Show)

instance Eq TType where
  TBool == TBool = True
  TChar == TChar = True
  TInt  == TInt  = True
  TVoid == TVoid = True
  TChar == TInt  = True
  TInt  == TChar = True
  TArray t1 == TArray t2 = t1 == t2
  (==) _ _ = False


type SymbolTable = M.Map Identifier TType
type FunctionArgumentTable = M.Map Identifier [TType]

data Tables = Tables { _symbols :: SymbolTable
                     , _functions :: FunctionArgumentTable
                     }

makeLenses ''Tables
