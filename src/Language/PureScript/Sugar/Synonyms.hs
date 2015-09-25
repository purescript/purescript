-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Synonyms
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module replaces fully applied type synonyms with the types which they
-- alias.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Sugar.Synonyms (
    desugarSynonyms
) where

import Data.Maybe (fromMaybe, mapMaybe)

import qualified Data.Map as M

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.AST
import Language.PureScript.Traversals
import Language.PureScript.Externs
import Language.PureScript.Types

type Synonyms = M.Map (Qualified ProperName) ([String], Type)

-- | Desugar all applied type synonyms into the types which they alias.
desugarSynonyms :: forall m. (Functor m, Applicative m, MonadError MultipleErrors m) => [ExternsFile] -> [Module] -> m [Module]
desugarSynonyms externs ms = mapM desugarSynonymsModule ms
  where
  allSynonyms :: Synonyms
  allSynonyms = M.union externsSynonyms moduleSynonyms

  moduleSynonyms :: Synonyms
  moduleSynonyms = M.fromList $ concatMap (\(Module _ _ mn ds _) -> mapMaybe (fromDeclaration mn) ds) ms
    where
    fromDeclaration :: ModuleName -> Declaration -> Maybe (Qualified ProperName, ([String], Type))
    fromDeclaration mn (TypeSynonymDeclaration pn args ty) = Just (Qualified (Just mn) pn, (map fst args, ty))
    fromDeclaration mn (PositionedDeclaration _ _ d) = fromDeclaration mn d
    fromDeclaration _ _ = Nothing

  externsSynonyms :: Synonyms
  externsSynonyms = M.fromList $ concatMap (\ExternsFile{..} -> mapMaybe (fromExternsDeclaration efModuleName) efDeclarations) externs
    where
    fromExternsDeclaration :: ModuleName -> ExternsDeclaration -> Maybe (Qualified ProperName, ([String], Type))
    fromExternsDeclaration mn EDTypeSynonym{..} = Just (Qualified (Just mn) edTypeSynonymName, (map fst edTypeSynonymArguments, edTypeSynonymType))
    fromExternsDeclaration _ _ = Nothing

  desugarSynonymsModule :: Module -> m Module
  desugarSynonymsModule (Module ss coms name ds exps) = Module ss coms name <$> mapM desugarSynonymsDecl ds <*> pure exps

  desugarSynonymsDecl :: Declaration -> m Declaration
  (desugarSynonymsDecl, _, _) = everywhereOnValuesTopDownM forDecls forValues pure
    where
    forDecls :: Declaration -> m Declaration
    forDecls (DataDeclaration dty pn args dctors) = DataDeclaration dty pn args <$> mapM (sndM (mapM forTypes)) dctors
    forDecls (ExternDeclaration nm ty) = ExternDeclaration nm <$> forTypes ty
    forDecls (TypeClassDeclaration pn args cs ds) = TypeClassDeclaration pn args <$> mapM (sndM (mapM forTypes)) cs <*> pure ds
    forDecls (TypeInstanceDeclaration nm cs cls tys body) = TypeInstanceDeclaration nm <$> mapM (sndM (mapM forTypes)) cs <*> pure cls <*> mapM forTypes tys <*> pure body
    forDecls (TypeSynonymDeclaration pn args ty) = TypeSynonymDeclaration pn args <$> forTypes ty
    forDecls (TypeDeclaration nm ty) = TypeDeclaration nm <$> forTypes ty
    forDecls other = pure other

    forValues :: Expr -> m Expr
    forValues (TypeClassDictionary cs tcds) = TypeClassDictionary <$> sndM (mapM forTypes) cs <*> pure tcds
    forValues (SuperClassDictionary cls tys) = SuperClassDictionary cls <$> mapM forTypes tys
    forValues (TypedValue ch val ty) = TypedValue ch val <$> forTypes ty
    forValues other = pure other

    forTypes :: Type -> m Type
    forTypes = everywhereOnTypesTopDownM forTypes'
      where
      forTypes' :: Type -> m Type
      forTypes' t = fromMaybe t <$> go 0 [] t

      go :: Int -> [Type] -> Type -> m (Maybe Type)
      go c args (TypeConstructor ctor)
        | Just (synArgs, body) <- M.lookup ctor allSynonyms
        , c == length synArgs
        = do let repl = replaceAllTypeVars (zip synArgs args) body
             Just <$> forTypes repl
        | Just (synArgs, _) <- M.lookup ctor allSynonyms
        , length synArgs > c
        = throwError . errorMessage $ PartiallyAppliedSynonym ctor
      go c args (TypeApp f arg) = go (c + 1) (arg : args) f
      go _ _ _ = return Nothing
