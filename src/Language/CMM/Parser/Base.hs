module Language.CMM.Parser.Base where
--
-- To get an actual parser One needs to define:
--    * expressionP     in term of baseExpressionP
--    * statementP      in terms of baseStatementP
--    * functionDefP    in terms of baseFunctionDefP
--    * funcP           in terms of baseFuncP
--    * progDataP       in terms of baseProgDataP
--    * programP        in terms of baseProgramP

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (Parsec)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isPrint)

import Language.CMM.AST

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
litCharP = do
  char '\''
  c <- satisfy f <|> try (string "\\0" >> return '\0') <|> (string "\\n" >> return '\n')
  symbol "'"
  return $ LitChar c
 where f c = isPrint c && c /= '\'' && c /= '\\'


litStringP :: MyParser Expression
litStringP = do
  char '"'
  s <- many $ (string "\\n" >> return '\n') <|> satisfy f
  symbol "\""
  return $ LitString s
 where f c = isPrint c && c /= '"'

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

baseStatementP :: MyParser Expression -> MyParser Statement
baseStatementP eP = returnP eP
         <|> ifP eP
         <|> assignP eP
         <|> forP eP
         <|> whileP eP
         <|> bracketedP eP
         <|> procedureCallP eP
         <|> noneP
         <?> "statement"

noneP :: MyParser Statement
noneP = semi >> return None

returnP :: MyParser Expression -> MyParser Statement
returnP eP = do
  reserved "return"
  e <- optionMaybe eP
  semi
  return $ Return e

ifP :: MyParser Expression -> MyParser Statement
ifP eP = do
  reserved "if"
  e <- parens eP
  ifS <- baseStatementP eP
  mElse <- optionMaybe $ reserved "else"
  case mElse of
       Nothing -> return $ If e ifS
       Just _  -> do elseS <- baseStatementP eP
                     return $ IfElse e ifS elseS

whileP :: MyParser Expression -> MyParser Statement
whileP eP = do
  reserved "while"
  e <- parens eP
  s <- baseStatementP eP
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

forP :: MyParser Expression -> MyParser Statement
forP eP = do
  reserved "for"
  symbol "("
  a1 <- optionMaybe $ assignmentP eP
  semi
  e <- optionMaybe eP
  semi
  a2 <- optionMaybe $ assignmentP eP
  symbol ")"
  s <- baseStatementP eP
  return $ For a1 e a2 s

bracketedP :: MyParser Expression -> MyParser Statement
bracketedP eP = do
  statements <- braces $ many (baseStatementP eP)
  return $ Bracketed statements

procedureCallP :: MyParser Expression -> MyParser Statement
procedureCallP eP = do
  FunctionCall f <- functionCallP eP
  semi
  return $ ProcedureCall f

--
-- Function Parsing
--

varDeclP :: MyParser VarDecl
varDeclP = do
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
  mArr <- optionMaybe (symbol "[" >> symbol "]")
  case mArr of
       Nothing -> return $ ScalarParam t i
       Just _  -> return $ ArrayParam  t i

nonVoidTypeP :: MyParser Type
nonVoidTypeP = (reserved "char" >> return Char) <|> (reserved "int" >> return Int)

typeP :: MyParser Type
typeP = nonVoidTypeP <|> (reserved "void" >> return Void)

baseFunctionDefP :: MyParser Expression -> MyParser FunctionDef
baseFunctionDefP eP = do
  t <- typeP
  i <- identifier
  p <- parens parametersP
  symbol "{"
  varDecls <- many varDeclP
  ss <- many $ baseStatementP eP
  symbol "}"
  return $ FunctionDef t i p varDecls ss

funcStubP :: MyParser FuncStub
funcStubP = liftM2 FuncStub identifier (parens parametersP)

--
-- Declaration
--

declarationP :: MyParser Declaration
declarationP = try functionDeclP
           <|> variableDeclP
           <?> "declaration"

variableDeclP :: MyParser Declaration
variableDeclP = liftM VariableDecl varDeclP

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

baseProgDataP :: MyParser Expression -> MyParser ProgData
baseProgDataP eP = try (baseFuncP eP) <|> declP

declP :: MyParser ProgData
declP = liftM Decl declarationP

baseFuncP :: MyParser Expression -> MyParser ProgData
baseFuncP eP = liftM Func (baseFunctionDefP eP)

baseProgramP :: MyParser Expression -> MyParser Program
baseProgramP eP = liftM Program (many $ baseProgDataP eP)
