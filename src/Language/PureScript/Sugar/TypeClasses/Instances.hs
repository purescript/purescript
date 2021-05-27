-- |
-- This module implements the desugaring pass which creates the compiler-generated
-- names for type class instances that do not have one defined in the source code.
--
module Language.PureScript.Sugar.TypeClasses.Instances
  ( desugarTypeClassInstanceNames
  ) where

import Prelude.Compat hiding (take)

import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Supply.Class
import           Data.Text (pack)
import           Language.PureScript.Errors
import           Language.PureScript.Names

-- |
-- Completes the name generation for type class instances that do not have
-- a unique name defined in source code. All `Left Text` values
-- will be converted to `Right Ident` values.
--
desugarTypeClassInstanceNames
  :: (MonadSupply m, MonadError MultipleErrors m)
  => Module
  -> m Module
desugarTypeClassInstanceNames (Module ss coms name decls exps) = do
  desugaredDecl <- parU decls desugarInstName
  pure $ Module ss coms name desugaredDecl exps

  where
  desugarInstName
    :: (MonadSupply m, MonadError MultipleErrors m)
    => Declaration
    -> m Declaration
  desugarInstName = \case
    TypeInstanceDeclaration sa chainId idx (Left genText) deps className tys bd -> do
      uniqueIdent <- fresh
      let finalName = Ident $ genText <> pack (show uniqueIdent)
      pure $ TypeInstanceDeclaration sa chainId idx (Right finalName) deps className tys bd
    a -> pure a
