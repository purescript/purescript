module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude

import Data.Function (id)
import Data.List (lookup)
import Language.PureScript.AST.Literals (Literal(ObjectLiteral))
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Bind(NonRec), Expr(App, Accessor, Let, ObjectUpdate, Var, Literal))
import Language.PureScript.CoreFn.Module (Module, moduleDecls)
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)
import Language.PureScript.Label (Label(Label))
import Language.PureScript.Names (ModuleName(ModuleName), ProperName(ProperName), Ident(UnusedIdent), Qualified(Qualified))
import Language.PureScript.Types (Type(TypeConstructor, RCons, REmpty, TypeApp))

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module Ann -> Module Ann
optimizeCoreFn m = m {moduleDecls = optimizeModuleDecls $ moduleDecls m}

optimizeModuleDecls :: [Bind Ann] -> [Bind Ann]
optimizeModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues id transformExprs id

  transformExprs = optimizeUnusedPartialFn . optimizeClosedRecordUpdate

-- |
-- Optimize away function generated to typecheck inferred Partial constraints.
--
optimizeUnusedPartialFn :: Expr a -> Expr a
optimizeUnusedPartialFn (Let _
  [NonRec _ UnusedIdent _]
  (App _ (App _ (Var _ (Qualified _ UnusedIdent)) _) originalCoreFn)) =
  originalCoreFn
optimizeUnusedPartialFn e = e

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
closedRecordFields :: Type -> Maybe [Label]
closedRecordFields (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Record"))) row) =
  collect row
  where
    collect :: Type -> Maybe [Label]
    collect REmpty = Just []
    collect (RCons l _ r) = collect r >>= return . (l :)
    collect _ = Nothing
closedRecordFields _ = Nothing
