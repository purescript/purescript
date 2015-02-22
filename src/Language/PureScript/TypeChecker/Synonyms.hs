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
import Control.Monad.Except
import Control.Monad.State

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- |
-- Build a type substitution for a type synonym
--
buildTypeSubstitution :: Qualified ProperName -> Int -> Type -> Either ErrorMessage (Maybe Type)
buildTypeSubstitution name n = go n []
  where
  go :: Int -> [Type] -> Type -> Either ErrorMessage (Maybe Type)
  go 0 args (TypeConstructor ctor) | name == ctor = return (Just $ SaturatedTypeSynonym ctor args)
  go m _ (TypeConstructor ctor) | m > 0 && name == ctor = throwError $ PartiallyAppliedSynonym name
  go m args (TypeApp f arg) = go (m - 1) (arg:args) f
  go _ _ _ = return Nothing

-- |
-- Replace all instances of a specific type synonym with the @SaturatedTypeSynonym@ data constructor
--
saturateTypeSynonym :: Qualified ProperName -> Int -> Type -> Either ErrorMessage Type
saturateTypeSynonym name n = everywhereOnTypesTopDownM replace
  where
  replace t = fromMaybe t <$> buildTypeSubstitution name n t

-- |
-- Replace all type synonyms with the @SaturatedTypeSynonym@ data constructor
--
saturateAllTypeSynonyms :: [(Qualified ProperName, Int)] -> Type -> Either ErrorMessage Type
saturateAllTypeSynonyms syns d = foldM (\result (name, n) -> saturateTypeSynonym name n result) d syns

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
    syns = map (\(name, (args, _)) -> (name, length args)) . M.toList $ typeSynonyms env
  in
    saturateAllTypeSynonyms syns d

replaceAllTypeSynonyms :: (e ~ MultipleErrors, Functor m, Monad m, MonadState CheckState m, MonadError e m) => Type -> m Type
replaceAllTypeSynonyms d = do
  env <- getEnv
  either (throwError . errorMessage) return $ replaceAllTypeSynonyms' env d

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
  either (throwError . errorMessage) return $ expandTypeSynonym' env name args

expandAllTypeSynonyms :: (e ~ MultipleErrors, Functor m, Applicative m, Monad m, MonadState CheckState m, MonadError e m) => Type -> m Type
expandAllTypeSynonyms = everywhereOnTypesTopDownM go
  where
  go (SaturatedTypeSynonym name args) = expandTypeSynonym name args
  go other = return other
