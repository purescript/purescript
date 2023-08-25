module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type, moduleName)

import Control.Monad.Supply (Supply)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.CSE (optimizeCommonSubexpressions)
import Language.PureScript.CoreFn.Expr (Bind, Expr(..))
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)
import Language.PureScript.Constants.Libs qualified as C

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module Ann -> Supply (Module Ann)
optimizeCoreFn m = fmap (\md -> m {moduleDecls = md}) . optimizeCommonSubexpressions (moduleName m) . optimizeModuleDecls $ moduleDecls m

optimizeModuleDecls :: [Bind Ann] -> [Bind Ann]
optimizeModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs
    = optimizeDataFunctionApply

-- | Optimize
-- `Data_Record.getField(Data_Record.hasFieldRecord(new Data_Symbol.IsSymbol(function() { return "f"; }))())(Type_Proxy.Proxy.value)(x)`
-- into
-- `x.f`
optimizeRecordGetField :: Expr a -> Expr a
optimizeRecordGetField
  (App ann
    (App _
      (App _
        (Var _ C.GetField)
        (App _
          (App _
            (Var _ C.HasFieldRecord)
            (App _
              (Var _ C.IsSymbolDict)
              (Literal _ (ObjectLiteral
                [ ("reflectSymbol", Abs _ _
                    (Literal _ (StringLiteral label)))
                ]))))
          _))
      (Var _ C.ProxyIdent))
    object) =
  Accessor ann label object
optimizeRecordGetField e = e

optimizeDataFunctionApply :: Expr a -> Expr a
optimizeDataFunctionApply e = case e of
  (App a (App _ (Var _ fn) x) y)
    | C.I_functionApply <- fn -> App a x y
    | C.I_functionApplyFlipped <- fn -> App a y x
  _ -> e
