module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude

import Data.Function (id)
import Language.PureScript.CoreFn.Expr (Bind(NonRec), Expr(App, Let, Var))
import Language.PureScript.CoreFn.Module (Module, moduleDecls)
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)
import Language.PureScript.Names (Ident(UnusedIdent), Qualified(Qualified))

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module a -> Module a
optimizeCoreFn m = m {moduleDecls = optimizeModuleDecls $ moduleDecls m}

optimizeModuleDecls :: [Bind a] -> [Bind a]
optimizeModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues id transformExprs id

  transformExprs = optimizeUnusedPartialFn

-- |
-- Optimize away function generated to typecheck inferred Partial constraints.
--
optimizeUnusedPartialFn :: Expr a -> Expr a
optimizeUnusedPartialFn (Let _
  [NonRec _ UnusedIdent _]
  (App _ (App _ (Var _ (Qualified _ UnusedIdent)) _) originalCoreFn)) =
  originalCoreFn
optimizeUnusedPartialFn e = e
