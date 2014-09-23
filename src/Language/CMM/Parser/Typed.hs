module Language.CMM.Parser.Typed where

import Language.CMM.Parser.Base
import Language.CMM.TypeChecker

expressionP   = baseExpressionP expressionP >>= typeCheckExpression
statementP    = baseStatementP expressionP statementP >>= typeCheckStatement
functionDefP  = baseFunctionDefP statementP >>= typeCheckFunctionDef
declarationP  = baseDeclarationP >>= typeCheckDeclaration True
progDataP     = baseProgDataP functionDefP declarationP
programP      = baseProgramP progDataP
