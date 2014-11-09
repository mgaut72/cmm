module Language.CMM.MIPS.Generate where

import Control.Lens
import Control.Monad
import Control.Applicative

import qualified Data.Map as M
import Data.Monoid
import Data.Char (ord)

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions
import Language.CMM.MIPS.Instructions


globalVars :: GenTable -> [DataDeclaration]
globalVars s = foldMapWithKey mkData $ s ^. globs
 where mkData i (TArray TInt (Just x)) = [DataItem i (4 * x)]
       mkData i (TArray TChar (Just x)) = [DataItem i x, Align 2]
       mkData i TInt = [DataItem i 4]
       mkData i TChar = [DataItem i 1]

foldMapWithKey f = M.foldlWithKey (\a k b -> a `mappend` f k b) mempty

loadIdentifier :: Identifier -> MIPSGen (Register, [Instruction])
loadIdentifier i = do
  glos <- use globs
  if i `M.member` glos
    then loadGlobal i
    else loadLocal i

loadGlobal :: Identifier -> MIPSGen (Register, [Instruction])
loadGlobal i = do
  glos <- use globs
  r <- getRegister
  case glos M.! i of
    TInt  -> return (r, [LoadWord r i])
    TChar -> return (r, [LoadByte r i])

loadLocal :: Identifier -> MIPSGen (Register, [Instruction])
loadLocal i = undefined

store :: Register -> Identifier -> MIPSGen [Instruction]
store r i = do
  glos <- use globs
  if i `M.member` glos
    then storeGlobal r i
    else storeLocal r i

storeGlobal r i = do
  glos <- use globs
  case glos M.! i of
    TInt  -> return [StoreWord r i]
    TChar -> return [StoreByte r i]

storeLocal r i = undefined


threeAddrToMips :: ThreeAddress -> MIPSGen [Instruction]

threeAddrToMips (AssignBinary i op v1 v2) = do
  (r1, i1) <- loadIdentifier v1
  (r2, i2) <- loadIdentifier v2
  r <- getRegister
  s <- store r i
  mapM_ freeRegister [r,r1,r2]
  return $ i1 <> i2 <> [getOp op r r1 r2] <> s
 where getOp Plus = Add
       getOp Minus = Sub
       getOp Times = Mult
       getOp Divide = Div

threeAddrToMips (AssignMinus i v) = do
  (srcR, srcCode) <- loadIdentifier v
  r <- getRegister
  s <- store r i
  mapM_ freeRegister [r,srcR]
  return $ srcCode <> [Neg r srcR] <> s

threeAddrToMips (Copy i val) = case val of
    IConst x -> doConst i x
    CConst c -> doConst i . toInteger . ord $  c
    IVar  iv -> do
      (r1, i1) <- loadIdentifier iv
      s <- store r1 i
      freeRegister r1
      return $ i1 <> s
 where doConst i x = do
         r <- getRegister
         s <- store r i
         freeRegister r
         return $ LoadImmed r x : s




