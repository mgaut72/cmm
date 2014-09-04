module Language.CMM.Syntax.AST where

data Expression = Negative      Expression
                | Not           Expression
                | LitInt        Integer
                | LitChar       Char
                | LitString     String
                | Binary        BinaryOp Expression Expression
                | Relative      RelativeOp Expression Expression
                | Logical       LogicalOp Expression Expression
                | FunctionCall  Identifier [Expression]
                | ArrayIndex    Identifier Expression
                | Var           Identifier
                deriving (Show, Eq)


-- data Function = Function Type Identifier Parameters Body

-- data Parameters = Void
--                 | Parameters [(Type, Variable)]

-- data Statement = If Expression Statement (Maybe Statement) -- Maybe for 'else'
--                | While Expression Statement
--                | For Assignment Expression Assignment Statement
--                | Return Expression
--                | Assign Assignment
--                | ProcedureCall Identifier [Expression] -- arguments
--                | Bracketed Statement
--                | None
--                deriving (Show)

-- data Variable = Scalar Identifier
--               | Array Identifier Expression

-- data Assignment = Assignment Variable Expression
--                 deriving (Show) -- Maybe for array index

type Identifier = String

data LogicalOp  = And | Or deriving (Show, Eq)
data RelativeOp = Eq | Neq | Leq | Less | Geq | Greater deriving (Show, Eq)
data BinaryOp   = Plus | Minus | Times | Divide  deriving (Show, Eq)


