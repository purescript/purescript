module Language.PureScript.Crash where

import Prelude.Compat

-- | Exit with an error message and a crash report link.
internalError :: String -> a
internalError =
  error
  . ("An internal error ocurred during compilation: " ++)
  . (++ "\nPlease report this at https://github.com/purescript/purescript/issues")
  . show
