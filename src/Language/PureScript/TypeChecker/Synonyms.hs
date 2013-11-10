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

{-# LANGUAGE Rank2Types #-}

module Language.PureScript.TypeChecker.Synonyms (
    saturateTypeSynonym,
    saturateAllTypeSynonyms
) where

import Language.PureScript.Types
import Language.PureScript.Declarations
import Language.PureScript.Names

import Data.Maybe (fromMaybe)
import Data.Data
import Data.Generics
import Control.Arrow
import Control.Monad.Writer
import Control.Monad.Error
import qualified Data.Map as M

buildTypeSubstitution :: Qualified ProperName -> Int -> Type -> Either String (Maybe Type)
buildTypeSubstitution name n = go n []
  where
  go :: Int -> [Type] -> Type -> Either String (Maybe Type)
  go 0 args (TypeConstructor ctor) | name == ctor = return (Just $ SaturatedTypeSynonym ctor args)
  go n _ (TypeConstructor ctor) | n > 0 && name == ctor = throwError $ "Partially applied type synonym " ++ show name
  go n args (TypeApp f arg) = go (n - 1) (arg:args) f
  go _ _ _ = return Nothing

everywhereM' :: (Monad m, Data d) => (forall d. (Data d) => d -> m d) -> d -> m d
everywhereM' f x = do
  y <- f x
  gmapM (everywhereM' f) y

saturateTypeSynonym :: (Data d) => Qualified ProperName -> Int -> d -> Either String d
saturateTypeSynonym name n = everywhereM' (mkM replace)
  where
  replace t = fmap (fromMaybe t) $ buildTypeSubstitution name n t

saturateAllTypeSynonyms :: (Data d) => [(Qualified ProperName, Int)] -> d -> Either String d
saturateAllTypeSynonyms syns d = foldM (\d (name, n) -> saturateTypeSynonym name n d) d syns



