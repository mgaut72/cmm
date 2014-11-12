module Language.CMM.MIPS.Memory where

import Control.Lens

import qualified Data.Map as M
import Data.Char (ord)
import Data.Monoid

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions
import Language.CMM.MIPS.Instructions

loadIdentifier :: Identifier -> MIPSGen (Register, [Instruction])
loadIdentifier i = locationAndType i >>= uncurry loadGeneral

loadGeneral :: Location -> TType -> MIPSGen (Register, [Instruction])
loadGeneral l t = do
  r <- getRegister
  return (r, [Comment $ "loading item at " ++ show l ++ "into regstr " ++ show r
             , (loadInstr t) r l])
 where loadInstr TChar = LoadByte
       loadInstr TInt  = LoadWord
       loadInstr (TArray TChar _) = LoadAddr
       loadInstr (TPointer _) = LoadWord
       loadInstr t = error $ "can't determine load command for " ++ show t ++
                             "in loadGeneral"

-- stores whatever is in r into the memory location of variable i
store :: Register -> Identifier -> MIPSGen [Instruction]
store r i = locationAndType i >>= uncurry (storeGeneral r)

storeOffset :: Register -> Identifier -> Value -> MIPSGen [Instruction]
storeOffset r i offset = do
  (l,t) <- locationAndType i
  baseT <- return $ case t of
    TPointer (TArray TInt _) -> TInt
    TPointer (TArray TChar _) -> TChar
    TArray TInt _ -> TInt
    TArray TChar _ -> TChar
  (offsetR,offsetIs) <- getVal offset
  newLocReg <- getRegister
  let adjustment = byteOffset offsetR baseT
  let newLoc = [ (loadInstr t) newLocReg l
               , Add newLocReg newLocReg offsetR]
  str <- storeGeneral r (Right (0, newLocReg)) baseT
  freeRegisters [offsetR, newLocReg]
  return $ offsetIs <> adjustment <> newLoc <> str
 where byteOffset reg t = if t == TInt then [ShiftLeft reg reg 2] else []
       loadInstr (TPointer _) = LoadWord
       loadInstr (TArray _ _) = LoadAddr



storeGeneral :: Register -> Location -> TType -> MIPSGen [Instruction]
storeGeneral r l t = return [(storeInstr t) r l]
 where storeInstr TInt = StoreWord
       storeInstr TChar = StoreByte
       storeInstr (TArray TChar _) = StoreWord
       storeInstr (TArray TInt _) = StoreWord
       storeInstr t = error $ "can't determine store command for " ++ show t ++
                             "in storeGeneral"

locationAndType :: Identifier -> MIPSGen (Location, TType)
locationAndType i = do
  ls <- use locs
  offsets <- use locOffsets
  glos <- use globs
  let (location, sTable) = if i `M.member` ls
                             then (Right (offsets M.! i), ls)
                             else (Left i, glos)
  let t = sTable M.! i
  return (location, t)

-- Either loads the const into a register or loads the variable based
-- on location
getVal :: Value -> MIPSGen (Register, [Instruction])
getVal (IConst x) = do
  r <- getRegister
  return (r, [ Comment ("putting " ++ show x ++ " int register " ++ show r)
             , LoadImmed r x])
getVal (CConst c) = getVal (IConst . toInteger . ord $ c)
getVal (IVar i) = loadIdentifier i
