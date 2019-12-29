-- | This module implements the desugaring pass which replaces ado-notation statements with
-- appropriate calls to pure and apply.

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.Sugar.AdoNotation (desugarAdoModule) where

import           Prelude.Compat hiding (abs)

import           Control.Monad (foldM)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Supply.Class
import           Data.List (foldl')
import           Language.PureScript.AST
import           Language.PureScript.Errors
import           Language.PureScript.Names
import qualified Language.PureScript.Constants as C

-- | Replace all @AdoNotationBind@ and @AdoNotationValue@ constructors with
-- applications of the pure and apply functions in scope, and all @AdoNotationLet@
-- constructors with let expressions.
desugarAdoModule :: forall m. (MonadSupply m, MonadError MultipleErrors m) => Module -> m Module
desugarAdoModule (Module ss coms mn ds exts) = Module ss coms mn <$> parU ds desugarAdo <*> pure exts

-- | Desugar a single ado statement
desugarAdo :: forall m. (MonadSupply m, MonadError MultipleErrors m) => Declaration -> m Declaration
desugarAdo d =
  let ss = declSourceSpan d
      (f, _, _) = everywhereOnValuesM return (replace ss) return
  in rethrowWithPosition ss $ f d
  where
  pure' :: SourceSpan -> Maybe ModuleName -> Expr
  pure' ss m = Var ss (Qualified m (Ident C.pure'))

  map' :: SourceSpan -> Maybe ModuleName -> Expr
  map' ss m = Var ss (Qualified m (Ident C.map))

  apply :: SourceSpan -> Maybe ModuleName -> Expr
  apply ss m = Var ss (Qualified m (Ident C.apply))

  replace :: SourceSpan -> Expr -> m Expr
  replace pos (Ado m els yield) = do
    (func, args) <- foldM (go pos) (yield, []) (reverse els)
    return $ case args of
      [] -> App (pure' pos m) func
      hd : tl -> foldl' (\a b -> App (App (apply pos m) a) b) (App (App (map' pos m) func) hd) tl
  replace _ (PositionedValue pos com v) = PositionedValue pos com <$> rethrowWithPosition pos (replace pos v)
  replace _ other = return other

  go :: SourceSpan -> (Expr, [Expr]) -> DoNotationElement -> m (Expr, [Expr])
  go _ (yield, args) (DoNotationValue val) =
    return (Abs NullBinder yield, val : args)
  go _ (yield, args) (DoNotationBind (VarBinder ss ident) val) =
    return (Abs (VarBinder ss ident) yield, val : args)
  go ss (yield, args) (DoNotationBind binder val) = do
    ident <- freshIdent'
    let abs = Abs (VarBinder ss ident)
                  (Case [Var ss (Qualified Nothing ident)]
                        [CaseAlternative [binder] [MkUnguarded yield]])
    return (abs, val : args)
  go _ (yield, args) (DoNotationLet ds) = do
    return (Let FromLet ds yield, args)
  go _ acc (PositionedDoNotationElement pos com el) =
    rethrowWithPosition pos $ do
      (yield, args) <- go pos acc el
      return $ case args of
        [] -> (PositionedValue pos com yield, args)
        (a : as) -> (yield, PositionedValue pos com a : as)
