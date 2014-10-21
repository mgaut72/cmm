module Language.CMM.Intermediate.Instructions where

import Language.CMM.AST

data Value = IConst Integer     -- TODO: or any other literal
           | IVar   Identifier
           deriving (Show, Eq)

type LabelName = String

data ThreeAddress = Global Identifier TType
                  | AssignBinary Identifier BinaryOp Value Value
                  | AssignMinus Identifier Value
                  | Copy Identifier Value
                  | Goto LabelName
                  | IIf Identifier RelativeOp Value LabelName
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
                  deriving (Show, Eq)
