module Language.CMM.Parser.UnTyped where

import Language.CMM.Parser.Base

expressionP     = baseExpressionP expressionP
statementP      = baseStatementP expressionP
functionDefP    = baseFunctionDefP expressionP
funcP           = baseFuncP expressionP
progDataP       = baseProgDataP expressionP
programP        = baseProgramP expressionP
