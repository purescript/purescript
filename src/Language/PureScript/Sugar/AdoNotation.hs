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
  let (f, _, _) = everywhereOnValuesM return replace return
  in f d
  where
  pure' :: Expr
  pure' = Var (Qualified Nothing (Ident C.pure'))

  map' :: Expr
  map' = Var (Qualified Nothing (Ident C.map))

  apply :: Expr
  apply = Var (Qualified Nothing (Ident C.apply))

  replace :: Expr -> m Expr
  replace (Ado els yield) = do
    (func, args) <- foldM go (yield, []) (reverse els)
    return $ case args of
      [] -> App pure' func
      hd : tl -> foldl' (\a b -> App (App apply a) b) (App (App map' func) hd) tl
  replace (PositionedValue pos com v) = PositionedValue pos com <$> rethrowWithPosition pos (replace v)
  replace other = return other

  go :: (Expr, [Expr]) -> DoNotationElement -> m (Expr, [Expr])
  go (yield, args) (DoNotationValue val) =
    return (Abs NullBinder yield, val : args)
  go (yield, args) (DoNotationBind (VarBinder ident) val) =
    return (Abs (VarBinder ident) yield, val : args)
  go (yield, args) (DoNotationBind binder val) = do
    ident <- freshIdent'
    let abs = Abs (VarBinder ident)
                  (Case [Var (Qualified Nothing ident)]
                        [CaseAlternative [binder] [MkUnguarded yield]])
    return (abs, val : args)
  go (yield, args) (DoNotationLet ds) = do
    return (Let ds yield, args)
  go acc (PositionedDoNotationElement pos com el) =
    rethrowWithPosition pos $ do
      (yield, args) <- go acc el
      return $ case args of
        [] -> (PositionedValue pos com yield, args)
        (a : as) -> (yield, PositionedValue pos com a : as)
