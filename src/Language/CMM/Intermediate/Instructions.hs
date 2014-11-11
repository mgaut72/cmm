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
                  | AssignBinary Identifier BinaryOp Identifier Identifier
                  | AssignMinus Identifier Identifier
                  | AssignNot Identifier Identifier
                  | Copy Identifier Value
                  | GoTo LabelName
                  | IIf Identifier RelativeOp Identifier LabelName LabelName
                  | Label LabelName
                  | AssignFromArr Identifier Identifier Integer -- should the second identifier be Value
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
                       , _parameters   :: [Identifier]
                       , _externs      :: S.Set Identifier
                       , _functionArgs :: FunctionArgumentTable
                       , _tempNum      :: Integer
                       , _currFcn      :: Identifier
                       } deriving (Show, Eq)

initialSymbols = Symbols M.empty M.empty [] S.empty M.empty 0 ""

type TACGen = State Symbols

tablesToSymbols :: Tables -> Identifier -> Symbols
tablesToSymbols t i = Symbols
  { _globals = t ^. globalSymbols
  , _locals = snd $ (t ^. localSymbols) M.! i
  , _parameters = getPs $ (t ^. localParameters) M.! i
  , _externs = t ^. externFunctions
  , _functionArgs = t ^. functions
  , _tempNum = 0
  , _currFcn = t ^. currFunction
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

typeOf (FunctionCall (Function i es)) = lookupSymb i

typeOf (Var (Scalar i)) = lookupSymb i

typeOf (Var (Array i e)) = lookupSymb i >>= (\(TArray t _) -> return t)

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

convertTo :: TType -> (Identifier, [ThreeAddress]) -> TACGen (Identifier, [ThreeAddress])
convertTo t (i, code) = do
  currT <- lookupSymb i
  if currT == t
    then return (i, code)
    else case (currT, t) of
           (TInt, TChar) -> convertIntToChar (i,code)
           (TChar, TInt) -> convertCharToInt (i,code)
           (TArray t1 _, TArray t2 _)
              | t1 == t2  -> return (i,code)
              | otherwise -> error $ "no coversion for " ++ show t1 ++ " to " ++ show t2
           (TArray t1 _, t2)
              | t1 == t2  -> return (i,code)
              | otherwise -> error $ "no coversion for " ++ show t1 ++ " to " ++ show t2
           (t1, TArray t2 _)
              | t1 == t2  -> return (i,code)
              | otherwise -> error $ "no coversion for " ++ show t1 ++ " to " ++ show t2
           _              ->error $ "no coversion for " ++ show currT ++ " to " ++ show t
-- TODO
convertCharToInt :: (Identifier, [ThreeAddress]) -> TACGen (Identifier, [ThreeAddress])
convertCharToInt = return

-- TODO
convertIntToChar :: (Identifier, [ThreeAddress]) -> TACGen (Identifier, [ThreeAddress])
convertIntToChar = return
