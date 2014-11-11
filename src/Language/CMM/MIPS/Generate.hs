module Language.CMM.MIPS.Generate where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.List (elemIndex)

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions
import Language.CMM.MIPS.Instructions
import Language.CMM.MIPS.Memory

generateLocal :: ([ThreeAddress], Symbols) -> MIPS
generateLocal (tas, s) = evalState mips gentable
  where mips = liftM (Instr . concat) . mapM threeAddrToMips $ tas
        gentable = symbolsToGenTable s

generateGlobal :: ([ThreeAddress], Symbols) -> MIPS
generateGlobal (_,s) = Data . globalVars . symbolsToGenTable $ s

globalVars :: GenTable -> [DataDeclaration]
globalVars s = foldMapWithKey mkData $ s ^. globs
 where mkData i t = if i `S.member` (s ^. fNames)
                      then []
                      else [DataItem i (sizeOf t), Align 2]
 -- align 2 might be overkill, but I think it doesn't hurt anything

foldMapWithKey f = M.foldlWithKey (\a k b -> a `mappend` f k b) mempty


-- Conversion functions

threeAddrToMips :: ThreeAddress -> MIPSGen [Instruction]

threeAddrToMips (AssignBinary i op v1 v2) = do
  (r1, i1) <- loadIdentifier v1
  (r2, i2) <- loadIdentifier v2
  r <- getRegister
  s <- store r i
  freeRegisters [r,r1,r2]
  return $ i1 <> i2 <> [getOp op r r1 r2] <> s
 where getOp Plus = Add
       getOp Minus = Sub
       getOp Times = Mult
       getOp Divide = Div

threeAddrToMips (AssignMinus i v) = do
  (srcR, srcCode) <- loadIdentifier v
  r <- getRegister
  s <- store r i
  freeRegisters [r,srcR]
  return $ srcCode <> [Neg r srcR] <> s

threeAddrToMips (Copy i val) = do
  (r, is) <- getVal val
  s <- store r i
  freeRegister r
  return $ is <> s

threeAddrToMips (AssignToArr arr offset val) = do
  (valReg, valCode) <- getVal val
  str <- storeOffset valReg arr offset
  freeRegister valReg
  return $ valCode <> str

threeAddrToMips (GoTo l) = return [Jump l]

threeAddrToMips (Label l) = return [Lab l]

threeAddrToMips (Enter i) = do
  (localSize, is) <- localVars
  return $ [ Comment $ "Start allocating stack for" ++ i
           , saveStack, saveReturn
           , newFrame, newStack localSize
           , Comment $ "End allocating stack for " ++ i] ++ is
  where saveStack  = StoreWord FP (Right (-4, SP))
        saveReturn = StoreWord RA (Right (-8, SP))
        newFrame   = LoadAddr  FP (Right (0, SP))
        newStack x = LoadAddr  SP (Right (-8-x, SP)) -- -8 for FP and RA

threeAddrToMips (Param i) = do
  (r,code) <- loadIdentifier i
  freeRegister r
  return $ code <> [StoreWord r (Right (-4, SP)), LoadAddr SP (Right (-4, SP))]

threeAddrToMips (Call f n) = return code
 where code = [JumpLink f, LoadAddr SP (Right (4 * n, SP))]

threeAddrToMips (Leave f) = threeAddrToMips (Ret Nothing)

threeAddrToMips (Ret Nothing) = return [resStack, resRet, resFrame, ret]
 where resStack = LoadAddr SP (Right (0, FP))
       resRet   = LoadWord RA (Right (-8, SP))
       resFrame = LoadWord FP (Right (-4, SP))
       ret      = JumpReturn RA

threeAddrToMips (Ret (Just i)) = do
  (r,code) <- loadIdentifier i
  freeRegister r
  ret <- threeAddrToMips (Ret Nothing)
  return $ code <> [Move V0 r] <> ret

threeAddrToMips (Retrieve i) = store V0 i

threeAddrToMips a = error $ "dont have " ++ show a ++ " implemented"

localVars :: MIPSGen (Integer, [Instruction])
localVars = use locs >>= \l -> sizeAndOffset 0 $ M.toList l
 where sizeAndOffset x [] = return (x, [])
       sizeAndOffset currOffset ((i,t):rest) = do
         ps <- use params
         case elemIndex i ps of
           Nothing -> do
             locOffsets %= M.insert i (currOffset, SP)
             (s,is) <- sizeAndOffset (currOffset + (align . sizeOf) t) rest
             return (s, is ++ [Comment $ i ++ " is at " ++ show (currOffset, SP)])
           Just x -> do
             locOffsets %= M.insert i (toInteger $ x * 4, FP)
             (s,is) <- sizeAndOffset currOffset rest
             return (s, is ++ [Comment $ i ++ " is at " ++ show (x*4, FP)])

align x
  | x `mod` 4 == 0 = x
  | otherwise      = x + 4 - (x `mod` 4)

sizeOf :: TType -> Integer
sizeOf TInt = 4
sizeOf TChar = 1
sizeOf (TArray TInt (Just x)) = 4 * x
sizeOf (TArray TChar (Just x)) = x
sizeOf x = error $ "cannot take the size of " ++ show x

-- "extern" functions

externs :: MIPS
externs = Instr $ [ Lab "print_int"
                  , LoadImmed V0 1
                  , LoadWord A0 (Right (0, SP))
                  , SysCall
                  , JumpReturn RA
                  , Comment "\n"
                  , Lab "print_string"
                  , LoadImmed V0 4
                  , LoadWord A0 (Right (0, SP))
                  , SysCall
                  , JumpReturn RA
                  ]
