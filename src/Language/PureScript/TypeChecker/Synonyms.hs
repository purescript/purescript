-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Synonyms
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Functions for replacing fully applied type synonyms with the @SaturatedTypeSynonym@ data constructor
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, GADTs #-}

module Language.PureScript.TypeChecker.Synonyms (
    saturateAllTypeSynonyms,
    desaturateAllTypeSynonyms,
    replaceAllTypeSynonyms,
    expandAllTypeSynonyms,
    expandTypeSynonym,
    expandTypeSynonym'
) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- |
-- Build a type substitution for a type synonym
--
buildTypeSubstitution :: M.Map (Qualified ProperName) Int -> Type -> Either ErrorMessage (Maybe Type)
buildTypeSubstitution m = go 0 []
  where
  go :: Int -> [Type] -> Type -> Either ErrorMessage (Maybe Type)
  go c args (TypeConstructor ctor) | M.lookup ctor m == Just c = return (Just $ SaturatedTypeSynonym ctor args)
  go c _    (TypeConstructor ctor) | M.lookup ctor m >  Just c = throwError $ SimpleErrorWrapper $ PartiallyAppliedSynonym ctor
  go c args (TypeApp f arg) = go (c + 1) (arg:args) f
  go _ _ _ = return Nothing

-- |
-- Replace all type synonyms with the @SaturatedTypeSynonym@ data constructor
--
saturateAllTypeSynonyms :: M.Map (Qualified ProperName) Int -> Type -> Either ErrorMessage Type
saturateAllTypeSynonyms syns = everywhereOnTypesTopDownM replace
  where
  replace t = fromMaybe t <$> buildTypeSubstitution syns t

-- |
-- \"Desaturate\" @SaturatedTypeSynonym@s
--
desaturateAllTypeSynonyms :: Type -> Type
desaturateAllTypeSynonyms = everywhereOnTypes replaceSaturatedTypeSynonym
  where
  replaceSaturatedTypeSynonym (SaturatedTypeSynonym name args) = foldl TypeApp (TypeConstructor name) args
  replaceSaturatedTypeSynonym t = t

-- |
-- Replace fully applied type synonyms with the @SaturatedTypeSynonym@ data constructor, which helps generate
-- better error messages during unification.
--
replaceAllTypeSynonyms' :: Environment -> Type -> Either ErrorMessage Type
replaceAllTypeSynonyms' env d =
  let
    syns = length . fst <$> typeSynonyms env
  in
    saturateAllTypeSynonyms syns d

replaceAllTypeSynonyms :: (e ~ MultipleErrors, Functor m, Monad m, MonadState CheckState m, MonadError e m) => Type -> m Type
replaceAllTypeSynonyms d = do
  env <- getEnv
  either (throwError . singleError) return $ replaceAllTypeSynonyms' env d

-- |
-- Replace a type synonym and its arguments with the aliased type
--
expandTypeSynonym' :: Environment -> Qualified ProperName -> [Type] -> Either ErrorMessage Type
expandTypeSynonym' env name args =
  case M.lookup name (typeSynonyms env) of
    Just (synArgs, body) -> do
      let repl = replaceAllTypeVars (zip (map fst synArgs) args) body
      replaceAllTypeSynonyms' env repl
    Nothing -> error "Type synonym was not defined"

expandTypeSynonym :: (e ~ MultipleErrors, Functor m, Monad m, MonadState CheckState m, MonadError e m) => Qualified ProperName -> [Type] -> m Type
expandTypeSynonym name args = do
  env <- getEnv
  either (throwError . singleError) return $ expandTypeSynonym' env name args

expandAllTypeSynonyms :: (e ~ MultipleErrors, Functor m, Applicative m, Monad m, MonadState CheckState m, MonadError e m) => Type -> m Type
expandAllTypeSynonyms = everywhereOnTypesTopDownM go
  where
  go (SaturatedTypeSynonym name args) = expandTypeSynonym name args
  go other = return other
