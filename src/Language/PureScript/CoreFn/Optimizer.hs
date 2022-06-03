module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import           Protolude                             hiding (Type)

import           Data.List                             (lookup)
import qualified Data.Text                             as T
import           Language.PureScript.AST.Literals
import           Language.PureScript.AST.SourcePos
import qualified Language.PureScript.Constants.Prelude as C
import qualified Language.PureScript.Constants.Prim    as C
import           Language.PureScript.CoreFn.Ann
import           Language.PureScript.CoreFn.Expr       as Expr
import           Language.PureScript.CoreFn.Module
import           Language.PureScript.CoreFn.Traversals
import           Language.PureScript.Label
import           Language.PureScript.Names             (Ident (..),
                                                        ModuleName (..),
                                                        Qualified (..))
import           Language.PureScript.Types

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
    = optimizeClosedRecordUpdate
    . optimizeDataFunctionApply
    . optimizeRecordGetField

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
    collect (RCons _ l _ r)    = (l :) <$> collect r
    collect _                  = Nothing
closedRecordFields _ = Nothing

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
  (App a (App _ (Var _ (Qualified (Just (ModuleName mn)) (Ident fn))) x) y)
    | mn == dataFunction && fn == C.apply -> App a x y
    | mn == dataFunction && fn == C.applyFlipped -> App a y x
  _ -> e
  where
  dataFunction :: Text
  dataFunction = T.replace "_" "." C.dataFunction
