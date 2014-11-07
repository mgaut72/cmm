module Language.CMM.Intermediate.FunctionCall where

import Control.Monad
import Control.Lens

import Data.Monoid
import Data.Maybe
import qualified Data.Map as M

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions
import Language.CMM.Intermediate.Expression

genFCall :: Function -> TACGen [ThreeAddress]
genFCall (Function  i es) = do
  paramCodes <- mapM genE es -- :: [(Identifier, [ThreeAddress])]
  argTypes <- liftM (fromJust . M.lookup i) (use functionArgs)
  convertedParams <- zipWithM convertTo argTypes paramCodes
  let allParams = foldl combine [] convertedParams
  return $ allParams <> [Call i (toInteger . length $ es)]
 where combine codes (ident,code) = codes <> code <> [Param (IVar ident)]
