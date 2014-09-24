module Language.CMM.Parser.UnTyped where

import Language.CMM.Parser.Base

expressionP   = baseExpressionP expressionP
statementP    = baseStatementP expressionP statementP
functionDefP  = baseFunctionDefP baseVarDeclP statementP
declarationP  = baseDeclarationP
progDataP     = baseProgDataP functionDefP declarationP
programP      = baseProgramP progDataP
