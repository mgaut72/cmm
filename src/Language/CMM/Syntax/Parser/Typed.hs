module Language.CMM.Syntax.Parser.Typed where

import Language.CMM.Syntax.Parser.Base
import Language.CMM.Syntax.TypeChecker

expressionP   = baseExpressionP expressionP >>= typeCheckExpression
statementP    = baseStatementP expressionP
functionDefP  = baseFunctionDefP expressionP
funcP         = baseFuncP expressionP
progDataP     = baseProgDataP expressionP
programP      = baseProgramP expressionP
