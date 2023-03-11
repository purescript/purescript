module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type, moduleName)

import Control.Monad.Supply (Supply)
import Data.List (lookup)
import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.CSE
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.Label
import Language.PureScript.Types
import Language.PureScript.Constants.Libs qualified as C
import Language.PureScript.Constants.Prim qualified as C

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
    = optimizeClosedRecordUpdate
    . optimizeDataFunctionApply

optimizeClosedRecordUpdate :: Expr Ann -> Expr Ann
optimizeClosedRecordUpdate ou@(ObjectUpdate a@(_, _, Just t, _) r updatedFields) =
  case closedRecordFields t of
    Nothing -> ou
    Just allFields -> Literal a (ObjectLiteral (map f allFields))
      where f (Label l) = case lookup l updatedFields of
              Nothing -> (l, Accessor (nullSourceSpan, [], Nothing, Nothing) l r)
              Just e -> (l, e)
optimizeClosedRecordUpdate e = e

-- | Return the labels of a closed record, or Nothing for other types or open records.
closedRecordFields :: Type a -> Maybe [Label]
closedRecordFields (TypeApp _ (TypeConstructor _ C.Record) row) =
  collect row
  where
    collect :: Type a -> Maybe [Label]
    collect (REmptyKinded _ _) = Just []
    collect (RCons _ l _ r) = (l :) <$> collect r
    collect _ = Nothing
closedRecordFields _ = Nothing

optimizeDataFunctionApply :: Expr a -> Expr a
optimizeDataFunctionApply e = case e of
  (App a (App _ (Var _ fn) x) y)
    | C.I_functionApply <- fn -> App a x y
    | C.I_functionApplyFlipped <- fn -> App a y x
  _ -> e
