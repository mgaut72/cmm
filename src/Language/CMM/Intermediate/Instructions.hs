{-# LANGUAGE TemplateHaskell #-}
module Language.CMM.Intermediate.Instructions where

import Control.Monad.State
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
                  | AssignBinary Identifier BinaryOp Identifier Identifier
                  | AssignMinus Identifier Identifier
                  | AssignNot Identifier Identifier
                  | Copy Identifier Value
                  | GoTo LabelName
                  | IIf Identifier RelativeOp Identifier LabelName LabelName
                  | Label LabelName
                  | AssignFromArr Identifier Identifier Value -- dest, arr, offset
                  | AssignToArr Identifier Value Value -- arr, offset, storage
                  | AssignAddress Identifier Identifier
                  | AssignPointer Identifier Identifier
                  | AssignToPointer Identifier Identifier
                  | Enter Identifier
                  | Leave Identifier
                  | Param Identifier
                  | Call Identifier Integer -- Integer is num arguments
                  | Ret (Maybe Identifier)
                  | Retrieve Identifier
                  | Convert Identifier TType
                  | NoOp
                  deriving (Show, Eq)

data Symbols = Symbols { _globals      :: SymbolTable
                       , _locals       :: SymbolTable
                       , _litStrings   :: M.Map Identifier String
                       , _parameters   :: [Identifier]
                       , _externs      :: S.Set Identifier
                       , _functionArgs :: FunctionArgumentTable
                       , _tempNum      :: Integer
                       , _currFcn      :: Identifier
                       } deriving (Show, Eq)

initialSymbols = Symbols M.empty M.empty M.empty [] S.empty M.empty 0 ""

type TACGen = State Symbols

tablesToSymbols :: Tables -> Identifier -> Symbols
tablesToSymbols t i = Symbols
  { _globals = t ^. globalSymbols
  , _locals = snd $ (t ^. localSymbols) M.! i
  , _litStrings = M.empty
  , _parameters = getPs $ (t ^. localParameters) M.! i
  , _externs = t ^. externFunctions
  , _functionArgs = t ^. functions
  , _tempNum = 0
  , _currFcn = i
  }


getPs VoidParameter = []
getPs (Parameters ps) = map getI ps
 where getI (ArrayParam _ i) = i
       getI (ScalarParam _ i) = i

makeLenses ''Symbols

getTmp :: TACGen Identifier
getTmp = do
  int <- tempNum <+= 1
  return $ "_tmp_" ++ show int

getLabel :: TACGen LabelName
getLabel = do
  f <- use currFcn
  int <- tempNum <+= 1
  return $ "_label_" ++ f ++ "_" ++ show int

typeOf :: Expression -> TACGen TType

typeOf ErrorE = return TError

typeOf LitInt{} = return TInt

typeOf LitChar{} = return TChar

typeOf LitString{} = return (TArray TChar Nothing)

typeOf Negative{} = return TInt

typeOf Not{} = return TBool

typeOf Binary{} = return TInt

typeOf Relative{} = return TBool

typeOf Logical{} = return TBool

typeOf (FunctionCall (Function i _)) = lookupSymb i

typeOf (Var (Scalar i)) = lookupSymb i

typeOf (Var (Array i _)) = lookupSymb i >>= (\(TArray t _) -> return t)

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

recordIdentifier :: TType -> Identifier -> TACGen Identifier
recordIdentifier t i = locals %= M.insert i t >> return i

recordLitString s = do
  int <- tempNum <+= 1
  f <- use currFcn
  let i = "_" ++ f ++ "_str_" ++ show int
  litStrings %= M.insert i s
  globals %= M.insert i (TArray TChar (Just . toInteger . length $ s))
  return (i,[])

convertTo :: TType -> (Identifier, [ThreeAddress]) -> TACGen (Identifier, [ThreeAddress])
convertTo _ = return
