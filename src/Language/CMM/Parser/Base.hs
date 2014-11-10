module Language.CMM.Parser.Base where
--
-- To get an actual parser One needs to define:
--    * expressionP     in term of baseExpressionP
--    * statementP      in terms of baseStatementP
--    * functionDefP    in terms of baseFunctionDefP
--    * funcP           in terms of baseFuncP
--    * progDataP       in terms of baseProgDataP
--    * declarationP    in terms of baseDeclarationP
--    * programP        in terms of baseProgramP

import Control.Monad
import Control.Applicative ((<*))
import Control.Lens
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Data.Char (isPrint)
import qualified Data.Map as M

import Language.CMM.AST
import Language.CMM.Error
import Language.CMM.TypeChecker

{-# ANN module "HLint: ignore Reduce duplication" #-}

type TypeChecked = Bool
type IsGlobal    = Bool

languageDef = Token.LanguageDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.nestedComments  = False
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> char '_'
  , Token.reservedNames   = [ "if"
                            , "else"
                            , "while"
                            , "for"
                            , "void"
                            , "char"
                            , "int"
                            , "return"
                            , "extern"
                            ]
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedOpNames = [ "+", "-", "*", "/", "<"
                            , ">" , "<=", ">=", "=="
                            , "!=", "||", "&&", "!"
                            ]
  , Token.caseSensitive   = True
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
comma      = Token.comma      lexer
whiteSpace = Token.whiteSpace lexer
brackets   = Token.brackets   lexer
commaSep   = Token.commaSep   lexer
commaSep1  = Token.commaSep1  lexer
braces     = Token.braces     lexer
lexeme     = Token.lexeme     lexer
symbol     = Token.symbol     lexer

--
-- Expression Parsing
--

baseExpressionP :: TypeChecked -> MyParser Expression
baseExpressionP b = untypedExpressionP >>= typeCheck b typeCheckExpression
 where untypedExpressionP = operationP b
                        <|> functionCallP b
                        <|> varExpressionP b
                        <|> litIntP
                        <|> litCharP
                        <|> litStringP
                        <?> "expression"

litIntP :: MyParser Expression
litIntP = liftM LitInt integer

litCharP :: MyParser Expression
litCharP = liftM LitChar $ char '\'' >> charInternal <* symbol "'"
 where charInternal = satisfy f <|> try nullChar <|> newlineChar
       f c = isPrint c && c /= '\'' && c /= '\\'
       nullChar = string "\\0" >> return '\0'


litStringP :: MyParser Expression
litStringP = liftM LitString $ char '"' >> strInternal <* symbol "\""
 where strInternal = many $ newlineChar <|> satisfy f
       f c = isPrint c && c /= '"'

newlineChar = string "\\n" >> return '\n'

operationP :: TypeChecked -> MyParser Expression
operationP b = buildExpressionParser operators (terms b)

operators = [ [ Prefix (reservedOp "-" >> return Negative)
              , Prefix (reservedOp "!" >> return Not)
              ]
            , [ Infix (reservedOp "*" >> return (Binary Times)) AssocLeft
              , Infix (reservedOp "/" >> return (Binary Divide)) AssocLeft
              ]
            , [ Infix (reservedOp "+" >> return (Binary Plus)) AssocLeft
              , Infix (reservedOp "-" >> return (Binary Minus)) AssocLeft
              ]
            , [ Infix (reservedOp ">" >> return (Relative Greater)) AssocLeft
              , Infix (reservedOp "<" >> return (Relative Less)) AssocLeft
              , Infix (reservedOp ">=" >> return (Relative Geq)) AssocLeft
              , Infix (reservedOp "<=" >> return (Relative Leq)) AssocLeft
              ]
            , [ Infix (reservedOp "==" >> return (Relative Eq)) AssocLeft
              , Infix (reservedOp "!=" >> return (Relative Neq)) AssocLeft
              ]
            , [ Infix (reservedOp "&&" >> return (Logical And)) AssocLeft
              , Infix (reservedOp "||" >> return (Logical Or)) AssocLeft
              ]
            ]

terms b = parens (baseExpressionP b)
    <|> functionCallP b
    <|> varExpressionP b
    <|> litIntP
    <|> litCharP
    <|> litStringP
    <?> "expression term"


functionCallP :: TypeChecked -> MyParser Expression
functionCallP b = do
  ident <- try $ identifierFollowedBy '('
  params <- commaSep $ baseExpressionP b
  symbol ")"
  return $ FunctionCall $ Function ident params

varExpressionP :: TypeChecked -> MyParser Expression
varExpressionP b = liftM Var (varP b)

varP b = arrayP b <|> scalarP b

scalarP :: TypeChecked -> MyParser Variable
scalarP b = liftM Scalar identifier

arrayP :: TypeChecked -> MyParser Variable
arrayP b = do
  ident <- try $ identifierFollowedBy '['
  idx <- baseExpressionP b
  symbol "]"
  return $ Array ident idx

identifierFollowedBy :: Char -> MyParser Identifier
identifierFollowedBy c = do
  i <- identifier
  symbol [c]
  return i


--
-- Statement Parser
--

baseStatementP :: TypeChecked -> MyParser Statement
baseStatementP b = (try validP <|> recoverableP) >>= tc
 where validP = returnP b
            <|> ifP b
            <|> procedureCallP b
            <|> assignP b
            <|> forP b
            <|> whileP b
            <|> bracketedP b
            <|> noneP
       recoverableP = ifErrorP b
                  <|> returnErrorP b
                  <|> try (forError1P b)
                  <|> forError2P b
                  <|> whileErrorP b
                  <|> assignErrorP b
                  <|> procedureCallErrorP b
       tc = typeCheck b typeCheckStatement

noneP :: MyParser Statement
noneP = semi >> return None

returnP :: TypeChecked -> MyParser Statement
returnP b = do
  reserved "return"
  e <- optionMaybe $ baseExpressionP b
  semi
  return $ Return e

ifP :: TypeChecked -> MyParser Statement
ifP b = do
  reserved "if"
  e <- parens $ baseExpressionP b
  ifOrIfElse b e


ifOrIfElse b e = do
  ifS <- baseStatementP b
  mElse <- optionMaybe $ reserved "else"
  case mElse of
       Nothing -> return $ If e ifS
       Just _  -> liftM (IfElse e ifS) (baseStatementP b)


whileP :: TypeChecked -> MyParser Statement
whileP b = do
  reserved "while"
  e <- parens $ baseExpressionP b
  s <- baseStatementP b
  return $ While e s

assignP :: TypeChecked -> MyParser Statement
assignP b = do
  a <- assignmentP b
  semi
  return $ Assign a

assignmentP :: TypeChecked -> MyParser Assignment
assignmentP b = do
  var <- varP b
  symbol "="
  e <- baseExpressionP b
  return $ Assignment var e

forP :: TypeChecked -> MyParser Statement
forP b = do
  reserved "for"
  symbol "("
  a1 <- optionMaybe $ assignmentP b
  semi
  e <- optionMaybe $ baseExpressionP b
  semi
  a2 <- optionMaybe $ assignmentP b
  symbol ")"
  s <- baseStatementP b
  return $ For a1 e a2 s

bracketedP :: TypeChecked -> MyParser Statement
bracketedP b = do
  statements <- braces $ many (baseStatementP b)
  return $ Bracketed statements

procedureCallP :: TypeChecked -> MyParser Statement
procedureCallP b = do
  FunctionCall f <- functionCallP b
  semi
  return $ ProcedureCall f

--
-- Function Parsing
--

baseVarDeclP :: IsGlobal -> TypeChecked -> MyParser VarDecl
baseVarDeclP g b = do
  t <- nonVoidTypeP
  vars <- commaSep1 $ arrayDeclP b <|> scalarP b
  semi
  when b (void $ typeCheckDeclaration g $ VariableDecl (VarDecl t vars))
  return $ VarDecl t vars

arrayDeclP :: TypeChecked -> MyParser Variable
arrayDeclP b = do
  ident <- try $ identifierFollowedBy '['
  size <- litIntP
  symbol "]"
  return $ Array ident size

parametersP :: MyParser Parameters
parametersP = voidParameterP <|> paramsP <?> "function parameters"

voidParameterP :: MyParser Parameters
voidParameterP = reserved "void" >> return VoidParameter

paramsP :: MyParser Parameters
paramsP = liftM Parameters (commaSep1 paramP)

paramP :: MyParser Parameter
paramP = do
  t <- nonVoidTypeP
  i <- identifier
  mArr <- optionMaybe $ symbol "[" >> symbol "]"
  case mArr of
       Nothing -> return $ ScalarParam t i
       Just _  -> return $ ArrayParam  t i

nonVoidTypeP :: MyParser TType
nonVoidTypeP = (reserved "char" >> return TChar)
           <|> (reserved "int" >> return TInt)

typeP :: MyParser TType
typeP = nonVoidTypeP <|> (reserved "void" >> return TVoid)

baseFunctionDefP :: TypeChecked -> MyParser FunctionDef
baseFunctionDefP b = do
  t <- typeP
  i <- identifier
  p <- parens parametersP
  modifyState $ currFunction .~ i
  modifyState $ localSymbols %~ M.insert i (t, M.empty)
  modifyState $ localParameters %~ M.insert i p
  when b $ checkSignature t i p >> addParameters p
  symbol "{"
  varDecls <- many $ baseVarDeclP False b
  ss <- many $ baseStatementP b
  when b $ checkReturns i t ss
  symbol "}"
  return $ FunctionDef t i p varDecls ss

funcStubP :: TypeChecked -> MyParser FuncStub
funcStubP b = liftM2 FuncStub identifier (parens parametersP)

--
-- Declaration
--

baseDeclarationP :: TypeChecked -> MyParser Declaration
baseDeclarationP b = try (functionDeclP b)
                 <|> variableDeclP b
                 <?> "declaration"

variableDeclP :: TypeChecked -> MyParser Declaration
variableDeclP b = liftM VariableDecl $ baseVarDeclP True b

functionDeclP :: TypeChecked ->  MyParser Declaration
functionDeclP b = do
  ext <- isExtP
  t <- typeP
  stubs <- commaSep1 $ funcStubP b
  semi
  typeCheck b (typeCheckDeclaration True) (FunctionDecl ext t stubs)

isExtP = option False (reserved "extern" >> return True)

--
-- ProgramData
--

baseProgDataP :: TypeChecked -> MyParser ProgData
baseProgDataP b = try (baseFuncP b) <|> declP b

declP :: TypeChecked -> MyParser ProgData
declP b = liftM Decl (baseDeclarationP b)

baseFuncP :: TypeChecked -> MyParser ProgData
baseFuncP b = liftM Func $ baseFunctionDefP b

baseProgramP :: TypeChecked -> MyParser Program
baseProgramP b = liftM Program $ many (baseProgDataP b)

--
-- Error Recover
--

errorUntil e p = manyTill anyChar (try $ lookAhead p) >> return e

ifErrorP :: TypeChecked -> MyParser Statement
ifErrorP b = do
  reserved "if"
  symbol "("
  recordError "bad expression in the conditional of the if statement"
  e <- errorUntil ErrorE $ char ')'
  symbol ")"
  ifOrIfElse b e

returnErrorP :: TypeChecked -> MyParser Statement
returnErrorP b = do
  reserved "return"
  recordError "bad return value expression"
  e <- errorUntil ErrorE $ char ';'
  semi
  return $ Return (Just e)

forError1P b = do
  reserved "for"
  symbol "("
  a1 <- try (optionMaybe (assignmentP b)) <|> err1
  semi
  e <- try (optionMaybe $ baseExpressionP b) <|> err2
  semi
  a2 <- try (optionMaybe (assignmentP b)) <|> err3
  symbol ")"
  s <- baseStatementP b
  return $ For a1 e a2 s
 where
   err1 = recordError "Bad first assignment in for statement" >> errorUntil (Just ErrorA) semi
   err2 = recordError "Bad expression in for statement" >> errorUntil (Just ErrorE) semi
   err3 = recordError "bad last assignment in for statement" >> errorUntil (Just ErrorA) (char ')')

forError2P b = do
  reserved "for"
  symbol "("
  errorUntil (Just ErrorA) (char ')')
  recordError "Bad for statement: unknown error inside the parens"
  symbol ")"
  s <- baseStatementP b
  return $ For (Just ErrorA) (Just ErrorE) (Just ErrorA) s

whileErrorP :: TypeChecked -> MyParser Statement
whileErrorP b = do
  reserved "while"
  symbol "("
  recordError "bad expression in the conditional of the while statement"
  e <- errorUntil ErrorE $ char ')'
  symbol ")"
  s <- baseStatementP b
  return $ While e s

assignErrorP b = do
  var <- varP b
  symbol "="
  recordError "bad assignment in the expression being assigned"
  e <- errorUntil ErrorE semi
  semi
  return $ Assign $ Assignment var e

procedureCallErrorP b = do
  ident <- try $ identifierFollowedBy '('
  recordError "bad parameters to procedure call"
  params <- errorUntil [ErrorE] $ char ')'
  symbol ")"
  semi
  return $ ProcedureCall $ Function ident params
