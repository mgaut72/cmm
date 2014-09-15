module Language.CMM.TypeChecker.Statement where

import Language.CMM.TypeChecker.Expression
import Language.CMM.TypeChecker.Assignment
import Language.CMM.AST
import Text.ParserCombinators.Parsec

typeCheckStatement :: Statement -> MyParser Statement

typeCheckStatement x@(If e s) = do
  est <- compatibleWith e TBool
  case est of
    Right TBool -> typeCheckStatement s
    Left  m     -> unexpected $ "Conditional of If statement must be of type Bool: " ++ m
  return x

typeCheckStatement x@(IfElse e s1 s2) = do
  est <- compatibleWith e TBool
  case est of
    Right TBool -> typeCheckStatement s1 >> typeCheckStatement s2
    Left  m     -> unexpected $ "Conditional of If statement must be of type Bool: " ++ m
  return x

typeCheckStatement x@(While e s) = do
  est <- compatibleWith e TBool
  case est of
    Right TBool -> typeCheckStatement s
    Left  m     -> unexpected $ "Conditional of While statement must be of type Bool: " ++ m
  return x

typeCheckStatement x@(For a1 e a2 s) = do
  case a1 of
    Just a  -> typeCheckAssignment a
    Nothing -> return ()
  case e of
    Just ex -> do
      est <- compatibleWith ex TBool
      case est of
        Right TBool -> return ()
        Left  m     -> unexpected $ "Conditional of For statement must be of type Bool: " ++ m
    Nothing -> return ()
  case a2 of
    Just a  -> typeCheckAssignment a
    Nothing -> return ()
  typeCheckStatement s
  return x


typeCheckStatement x@(Return e) = return x

typeCheckStatement None = return None

typeCheckStatement x@(Assign a) = typeCheckAssignment a >> return x

typeCheckStatement x@(Bracketed ss) = mapM_ typeCheckStatement ss >> return x
