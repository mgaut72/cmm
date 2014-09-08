module Language.CMM.Syntax.AST where

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


-- data Function = Function Type Identifier Parameters Body

-- data Parameters = Void
--                 | Parameters [(Type, Variable)]

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


