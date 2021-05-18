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
import           Language.PureScript.Types

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
  guardWith (MultipleErrors $ (M.toList duplMap >>= mkError)) $
    M.null duplMap
  let instMap = M.map (\(i, _, _, _) -> i) instMappings'
  pure (instMap, Module ss coms name desugared exps)

  where
  mkError :: (Text, [(SourceSpan, Qualified (ProperName 'ClassName), [SourceType])])
          -> [ErrorMessage]
  mkError (genText, vals) = (\(sourceSpan, className, tys) ->
      ErrorMessage [positionedError sourceSpan, ErrorInInstance className tys] $
        DuplicatePartiallyGeneratedInstanceName genText
    ) <$> vals

  f :: (SourceSpan, Qualified (ProperName 'ClassName), [SourceType], Text, Ident)
    -> (M.Map Text [(SourceSpan, Qualified (ProperName 'ClassName), [SourceType])], M.Map Text (Ident, SourceSpan, Qualified (ProperName 'ClassName), [SourceType]))
    -> (M.Map Text [(SourceSpan, Qualified (ProperName 'ClassName), [SourceType])], M.Map Text (Ident, SourceSpan, Qualified (ProperName 'ClassName), [SourceType]))
  f (sa, cls, tys, t, ident) (duplMap, instMap) = case M.lookup t instMap of
    Nothing -> (duplMap, M.insert t (ident, sa, cls, tys) instMap)
    Just (_, dupSa, dupCls, dupTys) -> do
      let addBothOnFirstTime = Just [(sa, cls, tys), (dupSa, dupCls, dupTys)]
          addAnotherDuplicate = Just . ((sa, cls, tys) :)
      (M.alter (maybe addBothOnFirstTime addAnotherDuplicate) t duplMap, instMap)

  desugarInstName
    :: (MonadSupply m, MonadError MultipleErrors m)
    => Declaration
    -> m (Declaration, Maybe (SourceSpan, Qualified (ProperName 'ClassName), [SourceType], Text, Ident))
  desugarInstName = \case
    TypeInstanceDeclaration sa chainId idx (Left genText) deps className tys bd -> do
      uniqueInt <- fresh
      -- truncate to 25 chars to reduce verbosity
      -- of name and still keep it readable
      let finalName = Ident $ (take 25 genText) <> "$" <> (pack $ show uniqueInt)
          decl = TypeInstanceDeclaration sa chainId idx (Right finalName) deps className tys bd
      pure (decl, Just (fst sa, className, tys, genText, finalName))
    a -> pure (a, Nothing)
