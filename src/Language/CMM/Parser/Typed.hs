module Language.CMM.Parser.Typed where

import Language.CMM.Parser.Base

tc = True

expressionP   = baseExpressionP tc
statementP    = baseStatementP tc
localVarDeclp = baseVarDeclP tc
functionDefP  = baseFunctionDefP tc
declarationP  = baseDeclarationP tc
progDataP     = baseProgDataP tc
programP      = baseProgramP tc
