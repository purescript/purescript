module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type)

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind, Expr(..))
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)
import Language.PureScript.Constants.Libs qualified as C

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module Ann -> Module Ann
optimizeCoreFn m = m {moduleDecls = optimizeModuleDecls $ moduleDecls m}

optimizeModuleDecls :: [Bind Ann] -> [Bind Ann]
optimizeModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs
    = optimizeDataFunctionApply

optimizeDataFunctionApply :: Expr a -> Expr a
optimizeDataFunctionApply e = case e of
  (App a (App _ (Var _ fn) x) y)
    | C.I_functionApply <- fn -> App a x y
    | C.I_functionApplyFlipped <- fn -> App a y x
  _ -> e
