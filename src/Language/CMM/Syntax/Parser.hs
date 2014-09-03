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

expressionP :: Parser Expression
expressionP = unaryOperationP
          <|> litIntP
          <|> litCharP
          <|> litStringP

litIntP :: Parser Expression
litIntP = liftM LitInt integer

litCharP :: Parser Expression
litCharP = do
  char '\''
  c <- satisfy f
  char '\''
  return $ LitChar c
 where f c = (isPrint c && c /= '\'' && c /= '\\') || c == '\0' || c == '\n'


litStringP :: Parser Expression
litStringP = do
  char '"'
  s <- many $ satisfy f
  char '"'
  return $ LitString s
 where f c = (isPrint c && c /= '"') || c == '\n'

unaryOperationP :: Parser Expression
unaryOperationP = buildExpressionParser unaryOperators unaryTerms

unaryOperators = [ [Prefix (reservedOp "-" >> return Negative)]
                 , [Prefix (reservedOp "!" >> return Not)]
                 ]

unaryTerms = litStringP <|> litCharP <|> litIntP
         <|> parens expressionP
