module Utils
  ( prettyPrint
  ) where

import Control.Monad.IO.Class

import Text.Pretty.Simple

prettyPrint :: (MonadIO m, Show a) => a -> m ()
prettyPrint =
  pPrintOpt
    CheckColorTty
    defaultOutputOptionsDarkBg {outputOptionsIndentAmount = 2}
