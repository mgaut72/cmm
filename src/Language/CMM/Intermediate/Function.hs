module Language.CMM.Intermediate.Function where

import Data.Monoid

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions
import Language.CMM.Intermediate.Expression

genF :: Function -> TACGen [ThreeAddress]
genF (Function  i es) = do
  paramCodes <- mapM genE es -- :: [(Identifier, [ThreeAddress])]
  argTypes <- use functionArgs >>= return . fromJust . M.lookup i -- :: [TType]
  convertedParams <- zipWithM convertTo argTypes paramCodes
  let allParams = foldl combine [] convertedParams
  return $ allParams <> [Call i (toInteger . length $ es)]
 where combine codes (ident,code) = codes <> code <> [Param (IVar ident)]
