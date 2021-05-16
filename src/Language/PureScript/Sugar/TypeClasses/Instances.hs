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
import qualified Data.Map as M
import           Data.Text (Text, take, pack)
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
  -> m (M.Map Text Ident, Module)
desugarTypeClassInstanceNames (Module ss coms name decls exps) = do
  (desugared, instMaps) <- unzip <$> parU decls desugarInstName
  pure (foldr M.union mempty instMaps, Module ss coms name desugared exps)

  where
  desugarInstName
    :: (MonadSupply m, MonadError MultipleErrors m)
    => Declaration
    -> m (Declaration, M.Map Text Ident)
  desugarInstName = \case
    TypeInstanceDeclaration sa chainId idx (Left genText) deps className tys bd -> do
      uniqueInt <- fresh
      -- truncate to 25 chars to reduce verbosity
      -- of name and still keep it readable
      let finalName = Ident $ (take 25 genText) <> "$" <> (pack $ show uniqueInt)
          decl = TypeInstanceDeclaration sa chainId idx (Right finalName) deps className tys bd
      pure (decl, M.singleton genText finalName)
    a -> pure (a, mempty)
