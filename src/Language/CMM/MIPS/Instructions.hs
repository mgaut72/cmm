{-# LANGUAGE TemplateHaskell #-}
module Language.CMM.MIPS.Instructions where

import Control.Lens
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions

data MIPS = Data [DataDeclaration]
          | Instr [Instruction]
          deriving (Show, Eq)

data DataDeclaration = DataItem Identifier Integer -- location, size
                     | Align Integer
                     | StringLiteral Identifier String
                     deriving (Show, Eq)

data Instruction = LoadWord    Register Location
                 | LoadByte    Register Location
                 | LoadAddr    Register Location
                 | LoadImmed   Register Integer
                 | StoreWord   Register Location
                 | StoreByte   Register Location
                 | Add         Register Register Register
                 | AddImmed    Register Register Integer
                 | Sub         Register Register Register
                 | Mult        Register Register Register
                 | Div         Register Register Register
                 | Neg         Register Register
                 | ShiftLeft   Register Register Integer
                 | Jump        LabelName
                 | JumpLink    LabelName
                 | JumpReturn  Register
                 | Lab         LabelName
                 | Move        Register Register
                 | Branch      RelativeOp Register Register LabelName
                 | Comment     String
                 | SysCall
                 deriving (Show, Eq)

type MIPSGen = State GenTable

data Register = Zero
              | AT
              | V0 | V1
              | A0 | A1 | A2 | A3
              | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7
              | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
              | T8 | T9
              | K0 | K1
              | GP
              | SP
              | FP
              | RA
              deriving (Show, Eq, Enum)

type Location = Either Identifier (Integer, Register)

data GenTable = GenTable { _globs      :: SymbolTable
                         , _locs       :: SymbolTable
                         , _params     :: [Identifier]
                         , _locOffsets :: M.Map Identifier (Integer, Register)
                         , _fNames     :: S.Set Identifier
                         , _registers  :: [Register]
                         , _stackAdjust :: Integer
                         } deriving (Show, Eq)

symbolsToGenTable :: Symbols -> GenTable
symbolsToGenTable s = GenTable { _globs = M.mapKeys underGlobVars (s ^. globals)
                               , _locs  = s ^. locals
                               , _params = s ^. parameters
                               , _locOffsets = M.empty
                               , _fNames = fNames'
                               , _registers = [T0 .. T7] ++ [T8,T9]
                               , _stackAdjust = 0
                               }
 where fNames' = S.union (s ^. externs) (S.fromList $ M.keys (s ^. functionArgs))
       nonGlobalVars = fNames' -- S.union fNames' $ M.keysSet (s ^. litStrings)
       underGlobVars i = if S.member i nonGlobalVars
                                then i
                                else '_':i

makeLenses ''GenTable

getRegister :: MIPSGen Register
getRegister = do
  (r:regs) <- use registers
  registers .= regs
  return r

freeRegister :: Register -> MIPSGen ()
freeRegister r = do
  regs <- use registers
  registers .= (r:regs)

freeRegisters :: [Register] -> MIPSGen ()
freeRegisters rs = do
  regs <- use registers
  registers .= rs ++ regs
