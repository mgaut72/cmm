module Language.CMM.Syntax.AST where

data Expression = Negative      Expression
                | Not           Expression
                | LitInt        Integer
                | LitChar       Char
                | LitString     String
                deriving (Show, Eq)
--                 | Relative      RelativeOp Expression Expression
--                 | Binary        BinaryOp Expression Expression
--                 | Logical       LogicalOp Expression Expression
--                 | FunctionCall  Identifier [Expression]
--                 | ArrayIndex    Identifier Expression
--                 | Grouped       Expression
--                 | Var           Variable


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

-- type Identifier = String

-- data LogicalOp  = And | Or deriving (Show)
-- data RelativeOp = Eq | Neq | Leq | Less | Geq | Greater deriving (Show)
-- data BinaryOp   = Plus | Minus | Times | Divide  deriving (Show)


