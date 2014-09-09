module Language.CMM.Syntax.Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isPrint)

import Language.CMM.Syntax.AST

languageDef = emptyDef { Token.commentStart    = "/*"
                       , Token.commentEnd      = "*/"
                       , Token.commentLine     = "//"
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
                       , Token.reservedOpNames = [ "+", "-", "*", "/", "<", ">"
                                                 , "<=", ">=", "==", "!=", "||"
                                                 , "&&", "!"
                                                 ]
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
expressionP :: Parser Expression
expressionP = operationP
          <|> functionCallP
          <|> varExpressionP
          <|> litIntP
          <|> litCharP
          <|> litStringP
          <?> "expression"

litIntP :: Parser Expression
litIntP = liftM LitInt integer

litCharP :: Parser Expression
litCharP = do
  char '\''
  c <- satisfy f <|> try (string "\\0" >> return '\0') <|> (string "\\n" >> return '\n')
  symbol "'"
  return $ LitChar c
 where f c = isPrint c && c /= '\'' && c /= '\\'


litStringP :: Parser Expression
litStringP = do
  char '"'
  s <- many $ (string "\\n" >> return '\n') <|> satisfy f
  symbol "\""
  return $ LitString s
 where f c = isPrint c && c /= '"'

operationP :: Parser Expression
operationP = buildExpressionParser operators terms

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

terms = parens expressionP
    <|> functionCallP
    <|> varExpressionP
    <|> litIntP
    <|> litCharP
    <|> litStringP
    <?> "expression term"


functionCallP :: Parser Expression
functionCallP = do
  ident <- try $ identifierFollowedBy '('
  params <- commaSep expressionP
  symbol ")"
  return $ FunctionCall $ Function ident params

varExpressionP :: Parser Expression
varExpressionP = liftM Var varP

varP = arrayP <|> scalarP

scalarP :: Parser Variable
scalarP = liftM Scalar identifier

arrayP :: Parser Variable
arrayP = do
  ident <- try $ identifierFollowedBy '['
  idx <- expressionP
  symbol "]"
  return $ Array ident idx

identifierFollowedBy :: Char -> Parser Identifier
identifierFollowedBy c = do
  i <- identifier
  symbol [c]
  return i


--
-- Statement Parser
--

statementP :: Parser Statement
statementP = returnP <|> ifP
         <|> assignP
         <|> forP
         <|> whileP
         <|> bracketedP
         <?> "statement"


returnP :: Parser Statement
returnP = do
  reserved "return"
  e <- optionMaybe expressionP
  semi
  return $ Return e

ifP :: Parser Statement
ifP = do
  reserved "if"
  e <- parens expressionP
  ifS <- statementP
  mElse <- optionMaybe $ reserved "else"
  case mElse of
       Nothing -> return $ If e ifS
       Just _  -> do elseS <- statementP
                     return $ IfElse e ifS elseS

whileP :: Parser Statement
whileP = do
  reserved "while"
  e <- parens expressionP
  s <- statementP
  return $ While e s

assignP :: Parser Statement
assignP = do
  a <- assignmentP
  semi
  return $ Assign a

assignmentP :: Parser Assignment
assignmentP = do
  var <- varP
  symbol "="
  e <- expressionP
  return $ Assignment var e

forP :: Parser Statement
forP = do
  reserved "for"
  symbol "("
  a1 <- optionMaybe assignmentP
  semi
  e <- optionMaybe expressionP
  semi
  a2 <- optionMaybe assignmentP
  symbol ")"
  s <- statementP
  return $ For a1 e a2 s

bracketedP :: Parser Statement
bracketedP = do
  statements <- braces $ many statementP
  return $ Bracketed statements

procedureCallP :: Parser Statement
procedureCallP = do
  FunctionCall f <- functionCallP
  semi
  return $ ProcedureCall f

--
-- Function Parsing
--

varDeclP :: Parser VarDecl
varDeclP = do
  t <- typeP
  vars <- commaSep1 $ arrayDeclP <|> scalarP
  semi
  return $ VarDecl t vars

arrayDeclP :: Parser Variable
arrayDeclP = do
  ident <- try $ identifierFollowedBy '['
  size <- litIntP
  symbol "]"
  return $ Array ident size

parametersP :: Parser Parameters
parametersP = voidP <|> paramsP <?> "function parameters"

voidP :: Parser Parameters
voidP = reserved "void" >> return Void

paramsP :: Parser Parameters
paramsP = liftM Parameters (commaSep1 paramP)

paramP :: Parser Parameter
paramP = do
  t <- typeP
  i <- identifier
  mArr <- optionMaybe (symbol "[" >> symbol "]")
  case mArr of
       Nothing -> return $ ScalarParam t i
       Just _  -> return $ ArrayParam  t i

typeP :: Parser Type
typeP = (reserved "char" >> return Char) <|> (reserved "int" >> return Int)

functionDefP :: Parser FunctionDef
functionDefP = typedFunctionDefP
           <|> voidFunctionDefP
           <?> "function definition"

typedFunctionDefP :: Parser FunctionDef
typedFunctionDefP = do
  t <- typeP
  generalFunctionDefP $ FunctionDef t


generalFunctionDefP f = do
  i <- identifier
  p <- parens parametersP
  symbol "{"
  varDecls <- many varDeclP
  ss <- many statementP
  symbol "}"
  return $ f i p varDecls ss

voidFunctionDefP :: Parser FunctionDef
voidFunctionDefP = do
  reserved "void"
  generalFunctionDefP VoidFunctionDef

funcStubP :: Parser FuncStub
funcStubP = liftM2 FuncStub identifier (parens parametersP)

--
-- Declaration
--

declarationP :: Parser Declaration
declarationP = variableDeclP
           <|> try functionDeclP
           <|> voidFunctionDeclP
           <?> "declaration"

variableDeclP :: Parser Declaration
variableDeclP = liftM VariableDecl varDeclP

functionDeclP :: Parser Declaration
functionDeclP = do
  ext <- isExtP
  t <- typeP
  stubs <- commaSep1 funcStubP
  semi
  return $ FunctionDecl ext t stubs

voidFunctionDeclP :: Parser Declaration
voidFunctionDeclP = do
  ext <- isExtP
  reserved "void"
  stubs <- commaSep1 funcStubP
  semi
  return $ VoidFunctionDecl ext stubs

isExtP = option False (reserved "extern" >> return True)

--
-- ProgramData
--

progDataP :: Parser ProgData
progDataP = try funcP <|> declP

declP :: Parser ProgData
declP = liftM Decl declarationP

funcP :: Parser ProgData
funcP = liftM Func functionDefP

programP :: Parser Program
programP = liftM Program (many progDataP)
