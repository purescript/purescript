-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.TypeDeclarations
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which replaces top-level type declarations with
-- type annotations on the corresponding expression.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Sugar.TypeDeclarations (
    desugarTypeDeclarationsModule
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad (forM)
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Traversals

-- |
-- Replace all top level type declarations in a module with type annotations
--
desugarTypeDeclarationsModule :: forall m. (Functor m, Applicative m, MonadError MultipleErrors m) => [Module] -> m [Module]
desugarTypeDeclarationsModule ms = forM ms $ \(Module ss coms name ds exps) ->
  rethrow (addHint (ErrorInModule name)) $
    Module ss coms name <$> desugarTypeDeclarations ds <*> pure exps
  where

  desugarTypeDeclarations :: [Declaration] -> m [Declaration]
  desugarTypeDeclarations (PositionedDeclaration pos com d : ds) = do
    (d' : ds') <- rethrowWithPosition pos $ desugarTypeDeclarations (d : ds)
    return (PositionedDeclaration pos com d' : ds')
  desugarTypeDeclarations (TypeDeclaration name ty : d : rest) = do
    (_, nameKind, val) <- fromValueDeclaration d
    desugarTypeDeclarations (ValueDeclaration name nameKind [] (Right (TypedValue True val ty)) : rest)
    where
    fromValueDeclaration :: Declaration -> m (Ident, NameKind, Expr)
    fromValueDeclaration (ValueDeclaration name' nameKind [] (Right val)) | name == name' = return (name', nameKind, val)
    fromValueDeclaration (PositionedDeclaration pos com d') = do
      (ident, nameKind, val) <- rethrowWithPosition pos $ fromValueDeclaration d'
      return (ident, nameKind, PositionedValue pos com val)
    fromValueDeclaration _ = throwError . errorMessage $ OrphanTypeDeclaration name
  desugarTypeDeclarations [TypeDeclaration name _] = throwError . errorMessage $ OrphanTypeDeclaration name
  desugarTypeDeclarations (ValueDeclaration name nameKind bs val : rest) = do
    let (_, f, _) = everywhereOnValuesTopDownM return go return
        f' (Left gs) = Left <$> mapM (pairM return f) gs
        f' (Right v) = Right <$> f v
    (:) <$> (ValueDeclaration name nameKind bs <$> f' val) <*> desugarTypeDeclarations rest
    where
    go (Let ds val') = Let <$> desugarTypeDeclarations ds <*> pure val'
    go other = return other
  desugarTypeDeclarations (d:ds) = (:) d <$> desugarTypeDeclarations ds
  desugarTypeDeclarations [] = return []
