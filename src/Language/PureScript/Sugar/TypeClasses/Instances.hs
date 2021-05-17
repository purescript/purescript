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
import           Data.Maybe (catMaybes)
import           Data.Text (Text, take, pack)
import           Language.PureScript.Errors
import           Language.PureScript.Names
import           Language.PureScript.TypeChecker.Monad

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
  (desugared, instMappings) <- unzip <$> parU decls desugarInstName
  let (duplMap, instMappings') = foldr f (mempty, mempty) $ catMaybes instMappings
  guardWith (MultipleErrors $ (uncurry mkError <$> M.toList duplMap)) $
    M.null duplMap
  let instMap = M.map snd instMappings'
  pure (instMap, Module ss coms name desugared exps)

  where
  mkError genText sourceSpans =
    ErrorMessage (positionedError <$> sourceSpans) $ DuplicatePartiallyGeneratedInstanceName genText

  f :: (SourceSpan, Text, Ident)
    -> (M.Map Text [SourceSpan], M.Map Text (SourceSpan, Ident))
    -> (M.Map Text [SourceSpan], M.Map Text (SourceSpan, Ident))
  f (sa, t, ident) (duplMap, instMap) = case M.lookup t instMap of
    Nothing -> (duplMap, M.insert t (sa, ident) instMap)
    Just (dupSa, _) -> do
      let addBothOnFirstTime = Just [sa, dupSa]
          addAnotherDuplicate = Just . (sa :)
      (M.alter (maybe addBothOnFirstTime addAnotherDuplicate) t duplMap, instMap)

  desugarInstName
    :: (MonadSupply m, MonadError MultipleErrors m)
    => Declaration
    -> m (Declaration, Maybe (SourceSpan, Text, Ident))
  desugarInstName = \case
    TypeInstanceDeclaration sa chainId idx (Left genText) deps className tys bd -> do
      uniqueInt <- fresh
      -- truncate to 25 chars to reduce verbosity
      -- of name and still keep it readable
      let finalName = Ident $ (take 25 genText) <> "$" <> (pack $ show uniqueInt)
          decl = TypeInstanceDeclaration sa chainId idx (Right finalName) deps className tys bd
      pure (decl, Just (fst sa, genText, finalName))
    a -> pure (a, Nothing)
