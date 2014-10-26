module Language.CMM.Parser.UnTyped where

import Language.CMM.Parser.Base

tc = False

expressionP   = baseExpressionP tc
statementP    = baseStatementP tc
localVarDeclp = baseVarDeclP tc
functionDefP  = baseFunctionDefP tc
declarationP  = baseDeclarationP tc
progDataP     = baseProgDataP tc
programP      = baseProgramP tc
