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
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Data.Char (isPrint)
import System.IO
import qualified Data.Map as M

import Language.CMM.AST
import Language.CMM.Error

{-# ANN module "HLint: ignore Reduce duplication" #-}

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

baseExpressionP :: MyParser Expression -> MyParser Expression
baseExpressionP eP = untypedExpressionP
 where untypedExpressionP = operationP eP
                        <|> functionCallP eP
                        <|> varExpressionP eP
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

operationP :: MyParser Expression -> MyParser Expression
operationP eP = buildExpressionParser operators (terms eP)

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

terms eP = parens eP
    <|> functionCallP eP
    <|> varExpressionP eP
    <|> litIntP
    <|> litCharP
    <|> litStringP
    <?> "expression term"


functionCallP :: MyParser Expression -> MyParser Expression
functionCallP eP = do
  ident <- try $ identifierFollowedBy '('
  params <- commaSep eP
  symbol ")"
  return $ FunctionCall $ Function ident params

varExpressionP :: MyParser Expression -> MyParser Expression
varExpressionP eP = liftM Var (varP eP)

varP eP = arrayP eP <|> scalarP

scalarP :: MyParser Variable
scalarP = liftM Scalar identifier

arrayP :: MyParser Expression -> MyParser Variable
arrayP eP = do
  ident <- try $ identifierFollowedBy '['
  idx <- eP
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

baseStatementP :: MyParser Expression -> MyParser Statement -> MyParser Statement
baseStatementP eP sP = try validP <|> recoverableP
 where validP = returnP eP
            <|> ifP eP sP
            <|> procedureCallP eP
            <|> assignP eP
            <|> forP eP sP
            <|> whileP eP sP
            <|> bracketedP sP
            <|> noneP
       recoverableP = ifErrorP eP sP
                  <|> returnErrorP
                  <|> try (forError1P eP sP)
                  <|> forError2P eP sP
                  <|> whileErrorP eP sP
                  <|> assignErrorP eP
                  <|> procedureCallErrorP eP

noneP :: MyParser Statement
noneP = semi >> return None

returnP :: MyParser Expression -> MyParser Statement
returnP eP = do
  reserved "return"
  e <- optionMaybe eP
  semi
  return $ Return e

ifP :: MyParser Expression -> MyParser Statement -> MyParser Statement
ifP eP sP = do
  reserved "if"
  e <- parens eP
  ifOrIfElse e eP sP


ifOrIfElse e eP sP = do
  ifS <- sP
  mElse <- optionMaybe $ reserved "else"
  case mElse of
       Nothing -> return $ If e ifS
       Just _  -> liftM (IfElse e ifS) sP


whileP :: MyParser Expression -> MyParser Statement -> MyParser Statement
whileP eP sP = do
  reserved "while"
  e <- parens eP
  s <- sP
  return $ While e s

assignP :: MyParser Expression -> MyParser Statement
assignP eP = do
  a <- assignmentP eP
  semi
  return $ Assign a

assignmentP :: MyParser Expression -> MyParser Assignment
assignmentP eP = do
  var <- varP eP
  symbol "="
  e <- eP
  return $ Assignment var e

forP :: MyParser Expression -> MyParser Statement -> MyParser Statement
forP eP sP = do
  reserved "for"
  symbol "("
  a1 <- optionMaybe $ assignmentP eP
  semi
  e <- optionMaybe eP
  semi
  a2 <- optionMaybe $ assignmentP eP
  symbol ")"
  s <- sP
  return $ For a1 e a2 s

bracketedP :: MyParser Statement -> MyParser Statement
bracketedP sP = do
  statements <- braces $ many sP
  return $ Bracketed statements

procedureCallP :: MyParser Expression -> MyParser Statement
procedureCallP eP = do
  FunctionCall f <- functionCallP eP
  semi
  return $ ProcedureCall f

--
-- Function Parsing
--

baseVarDeclP :: MyParser VarDecl
baseVarDeclP = do
  t <- nonVoidTypeP
  vars <- commaSep1 $ arrayDeclP <|> scalarP
  semi
  return $ VarDecl t vars

arrayDeclP :: MyParser Variable
arrayDeclP = do
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

baseFunctionDefP :: MyParser VarDecl -> MyParser Statement -> MyParser FunctionDef
baseFunctionDefP vP sP = do
  t <- typeP
  i <- identifier
  p <- parens parametersP
  modifyState $ currFunction .~ i
  modifyState $ localSymbols %~ M.insert i (t, M.empty)
  symbol "{"
  varDecls <- many vP
  ss <- many sP
  symbol "}"
  return $ FunctionDef t i p varDecls ss

funcStubP :: MyParser FuncStub
funcStubP = liftM2 FuncStub identifier (parens parametersP)

--
-- Declaration
--

baseDeclarationP :: MyParser Declaration
baseDeclarationP = try functionDeclP
               <|> variableDeclP
               <?> "declaration"

variableDeclP :: MyParser Declaration
variableDeclP = liftM VariableDecl baseVarDeclP

functionDeclP :: MyParser Declaration
functionDeclP = do
  ext <- isExtP
  t <- typeP
  stubs <- commaSep1 funcStubP
  semi
  return $ FunctionDecl ext t stubs

isExtP = option False (reserved "extern" >> return True)

--
-- ProgramData
--

baseProgDataP :: MyParser FunctionDef -> MyParser Declaration -> MyParser ProgData
baseProgDataP fP dP = try (baseFuncP fP) <|> declP dP

declP :: MyParser Declaration -> MyParser ProgData
declP = liftM Decl

baseFuncP :: MyParser FunctionDef -> MyParser ProgData
baseFuncP = liftM Func

baseProgramP :: MyParser ProgData -> MyParser Program
baseProgramP pdP = liftM Program $ many pdP

--
-- Error Recover
--

errorUntil e p = manyTill anyChar (try $ lookAhead p) >> return e

ifErrorP :: MyParser Expression -> MyParser Statement -> MyParser Statement
ifErrorP eP sP = do
  reserved "if"
  symbol "("
  recordError "bad expression in the conditional of the if statement"
  e <- errorUntil ErrorE $ char ')'
  symbol ")"
  ifOrIfElse e eP sP

returnErrorP :: MyParser Statement
returnErrorP = do
  reserved "return"
  recordError "bad return value expression"
  e <- errorUntil ErrorE $ char ';'
  semi
  return $ Return (Just e)

forError1P eP sP = do
  reserved "for"
  symbol "("
  a1 <- try (optionMaybe (assignmentP eP)) <|> err1
  semi
  e <- try (optionMaybe eP) <|> err2
  semi
  a2 <- try (optionMaybe (assignmentP eP)) <|> err3
  symbol ")"
  s <- sP
  return $ For a1 e a2 s
 where
   err1 = recordError "Bad first assignment in for statement" >> errorUntil (Just ErrorA) semi
   err2 = recordError "Bad expression expression in for statement" >> errorUntil (Just ErrorE) semi
   err3 = recordError "bad last assignment in for statement" >> errorUntil (Just ErrorA) (char ')')

forError2P eP sP = do
  reserved "for"
  symbol "("
  errorUntil (Just ErrorA) (char ')')
  recordError "Bad for statement: unknown error inside the parens"
  symbol ")"
  s <- sP
  return $ For (Just ErrorA) (Just ErrorE) (Just ErrorA) s

whileErrorP :: MyParser Expression -> MyParser Statement -> MyParser Statement
whileErrorP eP sP = do
  reserved "while"
  symbol "("
  recordError "bad expression in the conditional of the while statement"
  e <- errorUntil ErrorE $ char ')'
  symbol ")"
  s <- sP
  return $ While e s

assignErrorP eP = do
  var <- varP eP
  symbol "="
  recordError "bad assignment in the expression being assigned"
  e <- errorUntil ErrorE semi
  semi
  return $ Assign $ Assignment var e

procedureCallErrorP eP = do
  ident <- try $ identifierFollowedBy '('
  recordError "bad parameters to procedure call"
  params <- errorUntil [ErrorE] $ char ')'
  symbol ")"
  semi
  return $ ProcedureCall $ Function ident params
