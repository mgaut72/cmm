{-# LANGUAGE TemplateHaskell #-}
module Language.CMM.MIPS.Instructions where

import Control.Lens
import Control.Monad.State

import qualified Data.Map as M

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions

data MIPS = Data [DataDeclaration]
          | Instr Instruction
          deriving (Show, Eq)

data DataDeclaration = DataItem String Integer -- location, size
                     | Align Integer
                     deriving (Show, Eq)

data Instruction = LoadWord  Register Identifier
                 | LoadByte  Register Identifier
                 | LoadImmed Register Integer
                 | StoreWord Register Identifier
                 | StoreByte Register Identifier
                 | Add       Register Register Register
                 | Sub       Register Register Register
                 | Mult      Register Register Register
                 | Div       Register Register Register
                 | Neg       Register Register
                 deriving (Show, Eq)

type MIPSGen = State GenTable

type Register = Int

data GenTable = GenTable { _globs      :: SymbolTable
                         , _locs       :: SymbolTable
                         , _locOffsets :: M.Map Identifier Int
                         , _fArgs :: FunctionArgumentTable
                         , _registers    :: [Register]
                         } deriving (Show, Eq)

symbolsToGenTable :: Symbols -> GenTable
symbolsToGenTable s = GenTable { _globs = s ^. globals
                               , _locs  = s ^. locals
                               , _fArgs = s ^. functionArgs
                               , _registers = [8..15]
                               }

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

