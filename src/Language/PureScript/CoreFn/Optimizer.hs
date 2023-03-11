module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type, moduleName)

import Control.Monad.Supply (Supply)
import Data.List (lookup)
import Language.PureScript.AST.Literals ( Literal(ObjectLiteral) )
import Language.PureScript.AST.SourcePos ( nullSourceSpan )
import Language.PureScript.CoreFn.Ann ( Ann )
import Language.PureScript.CoreFn.CSE ( optimizeCommonSubexpressions )
import Language.PureScript.CoreFn.Expr ( Bind, Expr(App, ObjectUpdate, Accessor, Literal, Var) )
import Language.PureScript.CoreFn.Module ( Module(moduleDecls, moduleName) )
import Language.PureScript.CoreFn.Traversals ( everywhereOnValues )
import Language.PureScript.Label ( Label(Label) )
import Language.PureScript.Types ( pattern REmptyKinded, Type(TypeApp, TypeConstructor, RCons) )
import Language.PureScript.Constants.Libs qualified as CLibs
import Language.PureScript.Constants.Prim qualified as CPrim

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
closedRecordFields (TypeApp _ (TypeConstructor _ CPrim.Record) row) =
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
    | CLibs.I_functionApply <- fn -> App a x y
    | CLibs.I_functionApplyFlipped <- fn -> App a y x
  _ -> e
