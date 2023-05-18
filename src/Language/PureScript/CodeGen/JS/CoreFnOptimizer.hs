module  Language.PureScript.CodeGen.JS.CoreFnOptimizer (optimizeCoreFn) where

import Protolude ( Maybe(..), (<$>), identity, map )

import Data.List (lookup)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr ( Bind, Expr(Literal, ObjectUpdate, Accessor) )
import Language.PureScript.Label (Label(..))
import Language.PureScript.Types (pattern REmptyKinded, Type(..))
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Traversals (everywhereOnValues)

------
-- CoreFn Optimization for JS Codegen
--
--
optimizeCoreFn :: Module Ann -> Module Ann
optimizeCoreFn m = m { moduleDecls = optimizeModuleDecls m.moduleDecls}

optimizeModuleDecls :: [Bind Ann] -> [Bind Ann]
optimizeModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs
    = optimizeClosedRecordUpdate 

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