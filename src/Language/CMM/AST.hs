{-# LANGUAGE TemplateHaskell #-}
module Language.CMM.AST where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Control.Applicative
import Control.Monad.Writer
import Text.Parsec


-- Parser type
type MyParser a = ParsecT String Tables (Writer [String]) a


-- Language tree types
data Program = Program [ProgData] deriving (Show, Eq)

data ProgData = Decl Declaration
              | Func FunctionDef
              deriving (Show, Eq)

data Declaration = VariableDecl     VarDecl
                 | FunctionDecl     IsExtern TType [FuncStub]
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
                | ErrorE
                deriving (Show, Eq)


data FunctionDef = FunctionDef TType Identifier Parameters [VarDecl] [Statement]
                 deriving (Show, Eq)

data Parameters = VoidParameter
                | Parameters [Parameter]
                deriving (Show, Eq)

data Parameter = ScalarParam TType Identifier
               | ArrayParam  TType Identifier
               deriving (Show, Eq)

data VarDecl = VarDecl TType [Variable] deriving (Show, Eq)

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
               deriving (Show, Eq)

foldStatement f init [] = init
foldStatement f init (s:ss) = case s of
  If _ s'         -> foldStatement f init (s':ss)
  IfElse _ s' s'' -> foldStatement f init (s':s'':ss)
  While _ s'      -> foldStatement f init (s':ss)
  For _ _ _ s'    -> foldStatement f init (s':ss)
  Bracketed ss'   -> foldStatement f init (ss' ++ ss)
  otherwise       -> foldStatement f (f init s) ss

mapStatement :: (Statement -> a) -> [Statement] -> [a]
mapStatement f = foldStatement (\bs s -> f s:bs) []

data Variable = Scalar Identifier
              | Array Identifier Expression
              deriving (Show, Eq)

data Function = Function Identifier [Expression] deriving (Show, Eq)

data Assignment = Assignment Variable Expression
                | ErrorA
                deriving (Show, Eq)

type Identifier = String

data LogicalOp  = And | Or deriving (Show, Eq)

data RelativeOp = Eq | Neq | Leq | Less | Geq | Greater deriving (Show, Eq)

data BinaryOp = Plus | Minus | Times | Divide  deriving (Show, Eq)

data TType = TBool
           | TChar
           | TInt
           | TVoid
           | TArray TType
           | TError
           deriving (Show, Eq)

compatible TBool TBool = True
compatible TChar TChar = True
compatible TInt  TInt  = True
compatible TVoid TVoid = True
compatible TChar TInt  = True
compatible TInt  TChar = True
compatible (TArray t1) (TArray t2) = compatible t1 t2
compatible _ _ = False

--
-- Symbol Table
--

type SymbolTable = M.Map Identifier TType
type FunctionArgumentTable = M.Map Identifier [TType]

data Tables = Tables { _globalSymbols       :: SymbolTable
                     , _localSymbols        :: M.Map Identifier (TType, SymbolTable)
                     , _externFunctions     :: S.Set Identifier
                     , _functions           :: FunctionArgumentTable
                     , _currFunction        :: Identifier
                     } deriving (Show, Eq)

makeLenses ''Tables

initialTables = Tables { _globalSymbols       = M.empty
                       , _localSymbols        = M.empty
                       , _externFunctions     = S.empty
                       , _functions           = M.empty
                       , _currFunction        = ""
                       }

lTable :: Applicative f => MyParser ((SymbolTable -> f SymbolTable) -> Tables -> f Tables)
lTable = do
  s <- getState
  let currF = s ^. currFunction
  let localTableLens = localSymbols . ix currF . _2
  return localTableLens

gTable :: Applicative f => MyParser ((SymbolTable -> f SymbolTable) -> Tables -> f Tables)
gTable = return globalSymbols
