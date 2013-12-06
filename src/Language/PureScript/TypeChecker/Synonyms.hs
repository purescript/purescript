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
--
-----------------------------------------------------------------------------

module Language.PureScript.TypeChecker.Synonyms (
    saturateTypeSynonym,
    saturateAllTypeSynonyms
) where

import Language.PureScript.Types
import Language.PureScript.Names

import Data.Maybe (fromMaybe)
import Data.Data
import Data.Generics
import Data.Generics.Extras
import Control.Monad.Writer
import Control.Monad.Error

buildTypeSubstitution :: Qualified ProperName -> Int -> Type -> Either String (Maybe Type)
buildTypeSubstitution name n = go n []
  where
  go :: Int -> [Type] -> Type -> Either String (Maybe Type)
  go 0 args (TypeConstructor ctor) | name == ctor = return (Just $ SaturatedTypeSynonym ctor args)
  go m _ (TypeConstructor ctor) | m > 0 && name == ctor = throwError $ "Partially applied type synonym " ++ show name
  go m args (TypeApp f arg) = go (m - 1) (arg:args) f
  go _ _ _ = return Nothing

saturateTypeSynonym :: (Data d) => Qualified ProperName -> Int -> d -> Either String d
saturateTypeSynonym name n = everywhereM' (mkM replace)
  where
  replace t = fmap (fromMaybe t) $ buildTypeSubstitution name n t

saturateAllTypeSynonyms :: (Data d) => [(Qualified ProperName, Int)] -> d -> Either String d
saturateAllTypeSynonyms syns d = foldM (\result (name, n) -> saturateTypeSynonym name n result) d syns



