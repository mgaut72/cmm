module Language.CMM.Parser.Typed where

import Language.CMM.Parser.Base
import Language.CMM.AST
import Language.CMM.TypeChecker

expressionP   = baseExpressionP expressionP >>= typeCheckExpression
statementP    = baseStatementP expressionP (statementP >>= typeCheckStatement)
localVarDeclp = baseVarDeclP >>= typeCheckDeclaration False . VariableDecl >>= (\(VariableDecl (VarDecl x y)) -> return $ VarDecl x y)
functionDefP  = baseFunctionDefP localVarDeclp statementP >>= typeCheckFunctionDef
declarationP  = baseDeclarationP >>= typeCheckDeclaration True
progDataP     = baseProgDataP functionDefP declarationP
programP      = baseProgramP progDataP
