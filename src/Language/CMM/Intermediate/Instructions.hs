module Language.CMM.Intermediate.Instructions where

import Control.Monad.State
import Control.Applicative
import Control.Lens

import qualified Data.Map as M

import Language.CMM.AST

data Value = IConst Integer     -- TODO: or any other literal
           | CConst Char
           | IVar   Identifier
           deriving (Show, Eq)

type LabelName = String

data ThreeAddress = Global Identifier TType
                  | AssignBinary Identifier BinaryOp Value Value
                  | AssignMinus Identifier Value
                  | AssignNot Identifier Value
                  | Copy Identifier Value
                  | Goto LabelName
                  | IIf Identifier RelativeOp Value LabelName
                  | Label LabelName
                  | AssignFromArr Identifier Identifier Integer -- should the second identifier be Value
                  | AssignToArr Identifier Integer Value
                  | AssignAddress Identifier Identifier
                  | AssignPointer Identifier Identifier
                  | AssignToPointer Identifier Identifier
                  | Enter Identifier
                  | Leave Identifier
                  | Param Value
                  | Call Identifier Integer -- Integer is num arguments
                  | Ret (Maybe Value)
                  | Retrieve Identifier
                  | Convert Identifier TType
                  deriving (Show, Eq)

type TACGen = State Tables

localTable :: Applicative f => TACGen ((SymbolTable -> f SymbolTable) -> Tables -> f Tables)
localTable = do
  currF <- use currFunction
  return $ localSymbols . ix currF . _2

globalTable :: Applicative f => TACGen ((SymbolTable -> f SymbolTable) -> Tables -> f Tables)
globalTable = return globalSymbols

getTmp :: TACGen Identifier
getTmp = do
  int <- use tempNum
  tempNum += 1
  return $ '_' : show int


typeOf :: Expression -> TACGen TType

typeOf ErrorE = return TError

typeOf (LitInt _) = return TInt

typeOf (LitChar _) = return TChar

typeOf (LitString _) = return (TArray TChar)

typeOf (Negative _) = return TInt

typeOf (Not _) = return TBool

typeOf (Binary _ _ _) = return TInt

typeOf (Relative _ _ _) = return TBool

typeOf (Logical _ _ _) = return TBool

typeOf (FunctionCall (Function i es)) = lookupSymb i

typeOf (Var (Scalar i)) = lookupSymb i

typeOf (Var (Array i e)) = lookupSymb i >>= (\(TArray t) -> return t)

lookupSymb :: Identifier -> TACGen TType
lookupSymb i = do
  currF <- use currFunction
  locTab <- use $ localSymbols . ix currF . _2
  let tloc = M.lookup i locTab
  gloTab <- use globalSymbols
  let tglo = M.lookup i gloTab
  case (tloc, tglo) of
                 (Just t, _) -> return t
                 (_, Just t) -> return t
                 _           -> error "unexpected symbol"

