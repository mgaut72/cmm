module Language.CMM.MIPS.Instructions.PrettyPrint where

import Data.List
import Data.Char (toLower)

import Language.CMM.MIPS.Instructions

pretty :: MIPS -> String
pretty (Data ds) = ".data\n" ++ (unlines . map (fmt . pData) $ ds) ++ "\n"
pretty (Instr is) = ".text\n" ++ (unlines . map (fmt . pInstr) $ is) ++ "\n"

pData :: DataDeclaration -> String
pData (DataItem name size) = name ++ ": .space " ++ show size
pData (Align x) = ".align " ++ show x

pInstr (LoadWord r o) = "lw " ++ pReg r ++ ", " ++ pOffset o
pInstr (LoadByte r o) = "lb " ++ pReg r ++ ", " ++ pOffset o
pInstr (LoadAddr r o) = "la " ++ pReg r ++ ", " ++ pOffset o
pInstr (LoadImmed r i) = "li " ++ pReg r ++ ", " ++ show i
pInstr (StoreWord r o) = "sw " ++ pReg r ++ ", " ++ pOffset o
pInstr (StoreByte r o) = "sb " ++ pReg r ++ ", " ++ pOffset o
pInstr (Add r1 r2 r3) = "add " ++ (intercalate ", " . map pReg $ [r1,r2,r3])
pInstr (Sub r1 r2 r3) = "sub " ++ (intercalate ", " . map pReg $ [r1,r2,r3])
pInstr (Mult r1 r2 r3) = "mult " ++ (intercalate ", " . map pReg $ [r1,r2,r3])
pInstr (Div r1 r2 r3) = "div " ++ (intercalate ", " . map pReg $ [r1,r2,r3])
pInstr (Neg r1 r2) = "neg " ++ (intercalate ", " . map pReg $ [r1,r2])
pInstr (Jump l) = "j " ++ l
pInstr (JumpLink l) = "jal " ++ l
pInstr (JumpReturn r) = "jr " ++ pReg r
pInstr (Lab l) = l ++ ":"
pInstr (Move r1 r2) = "move " ++ (intercalate ", " . map pReg $ [r1,r2])
pInstr (Comment s) = "# " ++ s ++ "\n"
pInstr SysCall = "syscall"

pOffset :: Either String (Integer, Register) -> String
pOffset (Left i) = i
pOffset (Right (offset, r)) = show offset ++ "(" ++ pReg r ++ ")"

pReg :: Register -> String
pReg r = '$' : (map toLower . show $ r)

fmt :: String -> String
fmt s = "    " ++ s
