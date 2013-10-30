-----------------------------------------------------------------------------
--
-- Module      :  PureScript.TypeChecker.Synonyms
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

module PureScript.TypeChecker.Synonyms (
    substituteTypeSynonym
) where

import PureScript.Types
import PureScript.Declarations

import Data.Maybe (fromMaybe)
import Data.Data
import Data.Generics
import Control.Arrow
import Control.Monad.Writer
import Control.Monad.Error
import qualified Data.Map as M

replaceTypeVars :: M.Map String Type -> Type -> Type
replaceTypeVars m = everywhere (mkT replace)
  where
  replace :: Type -> Type
  replace t@(TypeVar var) = fromMaybe t (M.lookup var m)
  replace t = t

buildTypeSubstitution :: String -> Type -> [String] -> Type -> Either String (Maybe Type)
buildTypeSubstitution name ty args = go (reverse args) M.empty
  where
  go :: [String] -> M.Map String Type -> Type -> Either String (Maybe Type)
  go [] m (TypeConstructor ctor) | name == ctor = return (Just $ replaceTypeVars m ty)
  go (_:_) _ (TypeConstructor ctor) | name == ctor = throwError $ "Partially applied type synonym " ++ name
  go (arg:args) m (TypeApp f x) = go args (M.insert arg x m) f
  go _ _ _ = return Nothing

everywhereM' :: (Monad m, Data d) => (forall d. (Data d) => d -> m d) -> d -> m d
everywhereM' f x = do
  y <- f x
  gmapM (everywhereM' f) y

substituteTypeSynonym :: (Data d) => String -> [String] -> Type -> d -> Either String d
substituteTypeSynonym name args ty = everywhereM' (mkM replace)
  where
  replace t = fmap (fromMaybe t) $ buildTypeSubstitution name ty args t
