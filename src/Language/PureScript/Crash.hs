module Language.PureScript.Crash (HasCallStack, internalError) where

import Prelude

import GHC.Stack (HasCallStack)

-- | Exit with an error message and a crash report link.
internalError :: HasCallStack => String -> a
internalError =
  error
  . ("An internal error occurred during compilation: " ++)
  . (++ "\nPlease report this at https://github.com/purescript/purescript/issues")
