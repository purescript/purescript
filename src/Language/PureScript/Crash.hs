{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}

module Language.PureScript.Crash where

import Prelude.Compat

import qualified GHC.Stack

-- | A compatibility wrapper for the @GHC.Stack.HasCallStack@ constraint.
#if __GLASGOW_HASKELL__ >= 800
type HasCallStack = GHC.Stack.HasCallStack
#elif MIN_VERSION_GLASGOW_HASKELL(7,10,2,0)
type HasCallStack = (?callStack :: GHC.Stack.CallStack)
#else
import GHC.Exts (Constraint)
-- CallStack wasn't present in GHC 7.10.1
type HasCallStack = (() :: Constraint)
#endif

-- | Exit with an error message and a crash report link.
internalError :: HasCallStack => String -> a
internalError =
  error
  . ("An internal error occurred during compilation: " ++)
  . (++ "\nPlease report this at https://github.com/purescript/purescript/issues")
  . show
