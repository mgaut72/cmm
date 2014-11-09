module Language.CMM.MIPS.Generate where

import Control.Lens

import qualified Data.Map as M
import Data.Monoid

import Language.CMM.AST
import Language.CMM.Intermediate.Instructions

data DataDeclaration = Data String Integer -- location, size
                     | Align Integer
                     deriving (Show, Eq)

globalVars :: Symbols -> [DataDeclaration]
globalVars s = foldMapWithKey mkData $ s ^. globals
 where mkData i (TArray TInt (Just x)) = [Data i (4 * x)]
       mkData i (TArray TChar (Just x)) = [Data i x, Align 2]
       mkData i TInt = [Data i 4]
       mkData i TChar = [Data i 1]

foldMapWithKey f = M.foldlWithKey (\a k b -> a `mappend` f k b) mempty
