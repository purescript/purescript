module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude

import Data.Function (id)
import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.CoreFn.Module (Module, moduleDecls)

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module a -> Module a
optimizeCoreFn m = m {moduleDecls = optimizeModuleDecls $ moduleDecls m}

optimizeModuleDecls :: [Bind a] -> [Bind a]
optimizeModuleDecls = id
