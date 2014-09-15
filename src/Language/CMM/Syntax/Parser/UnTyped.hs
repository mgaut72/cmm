module Language.CMM.Syntax.Parser.UnTyped where

import Language.CMM.Syntax.Parser.Base

expressionP     = baseExpressionP expressionP
statementP      = baseStatementP expressionP
functionDefP    = baseFunctionDefP expressionP
funcP           = baseFuncP expressionP
progDataP       = baseProgDataP expressionP
programP        = baseProgramP expressionP
