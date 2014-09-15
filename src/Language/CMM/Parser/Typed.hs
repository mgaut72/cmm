module Language.CMM.Parser.Typed where

import Language.CMM.Parser.Base
import Language.CMM.TypeChecker.Expression

expressionP   = baseExpressionP expressionP >>= typeCheckExpression
statementP    = baseStatementP expressionP
functionDefP  = baseFunctionDefP expressionP
funcP         = baseFuncP expressionP
progDataP     = baseProgDataP expressionP
programP      = baseProgramP expressionP
