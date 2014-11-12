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
  (l,TArray t _) <- locationAndType i
  (offsetR,offsetIs) <- getVal offset
  newLocReg <- getRegister
  let adjustment = if t == TInt then [Comment $ i ++ " is type int, so its offset (in register " ++ show offsetR ++ " needs to be multiplied by 4",ShiftLeft offsetR offsetR 2] else []
  -- at this point we have converted the index into the byte offset
  let newLoc = [ LoadAddr newLocReg l
               , Add newLocReg newLocReg offsetR]
  str <- storeGeneral r (Right (0, newLocReg)) t
  freeRegisters [offsetR, newLocReg]
  return $ offsetIs <> adjustment <> newLoc <> str

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
