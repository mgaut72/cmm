{-# LANGUAGE TemplateHaskell #-}
module Language.CMM.Intermediate.Instructions where

import Control.Monad.State
import Control.Applicative
import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S

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
                  | GoTo LabelName
                  | IIf Identifier RelativeOp Identifier LabelName LabelName
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
                  | NoOp
                  deriving (Show, Eq)

data Symbols = Symbols { _globals      :: SymbolTable
                       , _locals       :: SymbolTable
                       , _externs      :: S.Set Identifier
                       , _functionArgs :: FunctionArgumentTable
                       , _tempNum      :: Integer
                       } deriving (Show, Eq)

initialSymbols = Symbols M.empty M.empty S.empty M.empty 0

type TACGen = State Symbols

tablesToSymbols :: Tables -> Identifier -> Symbols
tablesToSymbols t i = Symbols { _globals = t ^. globalSymbols
                              , _locals = snd $ (t ^. localSymbols) M.! i
                              , _externs = t ^. externFunctions
                              , _functionArgs = t ^. functions
                              , _tempNum = 0
                              }

makeLenses ''Symbols

getTmp :: TACGen Identifier
getTmp = do
  int <- use tempNum
  tempNum += 1
  return $ "_tmp_" ++ show int

getLabel :: TACGen LabelName
getLabel = do
  int <- use tempNum
  tempNum += 1
  return $ "_label_" ++ show int

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
  gloTab <- use globals
  locTab <- use locals
  let tglo = M.lookup i gloTab
  let tloc = M.lookup i locTab
  case (tloc, tglo) of
    (Just t, _) -> return t
    (_, Just t) -> return t
    _           -> error "unexpected symbol"

recordIdentifier :: Identifier -> TType -> TACGen ()
recordIdentifier i t = locals %= M.insert i t

convertTo :: TType -> (Identifier, [ThreeAddress]) -> TACGen (Identifier, [ThreeAddress])
convertTo t (i, code) = do
  currT <- lookupSymb i
  if currT == t
    then return (i, code)
    else case (currT, t) of
           (TInt, TChar) -> convertIntToChar (i,code)
           (TChar, TInt) -> convertCharToInt (i,code)

-- TODO
convertCharToInt :: (Identifier, [ThreeAddress]) -> TACGen (Identifier, [ThreeAddress])
convertCharToInt = return

-- TODO
convertIntToChar :: (Identifier, [ThreeAddress]) -> TACGen (Identifier, [ThreeAddress])
convertIntToChar = return
