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
                                                 , "return"
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
whiteSpace = Token.whiteSpace lexer
brackets   = Token.brackets   lexer
commaSep   = Token.commaSep   lexer
braces     = Token.braces     lexer
lexeme     = Token.lexeme     lexer

--
-- Expression Parsing
--
expressionP :: Parser Expression
expressionP = operationP
          <|> functionCallP
          <|> arrayIndexP
          <|> varP
          <|> litIntP
          <|> litCharP
          <|> litStringP

litIntP :: Parser Expression
litIntP = liftM LitInt integer

litCharP :: Parser Expression
litCharP = do
  char '\''
  c <- satisfy f <|> try (string "\\0" >> return '\0') <|> (string "\\n" >> return '\n')
  char '\''
  return $ LitChar c
 where f c = isPrint c && c /= '\'' && c /= '\\'


litStringP :: Parser Expression
litStringP = do
  char '"'
  s <- many $ (string "\\n" >> return '\n') <|> satisfy f
  char '"'
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
    <|> arrayIndexP
    <|> varP
    <|> litIntP
    <|> litCharP
    <|> litStringP


functionCallP :: Parser Expression
-- didnt use:
--     functionCallP = liftM2 FunctionCall identifier (parens $ commaSep expressionP)
-- so that I can avoid having "try" at the expressionP level
functionCallP = do
  ident <- try (do {i <- identifier; lexeme $ char '('; return i})
  params <- commaSep $ lexeme expressionP
  lexeme $ char ')'
  return $ FunctionCall ident params

arrayIndexP :: Parser Expression
-- arrayIndexP = liftM2 ArrayIndex identifier (brackets expressionP)
-- again, avoiding "try" at the expressionP level
arrayIndexP = do
  ident <- try (do {i <- identifier; lexeme $ char '['; return i})
  idx <- expressionP
  lexeme $ char ']'
  return $ Var (Array ident idx)


varP :: Parser Expression
varP = liftM (Var . Scalar) identifier

--
-- Statement Parser
--

statementP :: Parser Statement
statementP = returnP
         <|> ifP
         <|> assignP
         <|> forP
         <|> bracketedP

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
  Var var <- arrayIndexP <|> varP
  lexeme $ char '='
  e <- expressionP
  return $ Assignment var e

forP :: Parser Statement
forP = do
  reserved "for"
  lexeme $ char '('
  a1 <- optionMaybe assignmentP
  semi
  e <- optionMaybe expressionP
  semi
  a2 <- optionMaybe assignmentP
  lexeme $ char ')'
  s <- statementP
  return $ For a1 e a2 s

bracketedP :: Parser Statement
bracketedP = do
  statements <- braces $ many statementP
  return $ Bracketed statements
