import Language.Haskell.HLint
import Control.Monad
import System.Exit

main = do
  srcHints <- hlint ["src"]
  testHints <- hlint ["tests"]
  errorIfNotEmpty srcHints
  errorIfNotEmpty testHints

errorIfNotEmpty hs = unless (null hs) exitFailure
