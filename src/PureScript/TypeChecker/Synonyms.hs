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

buildTypeSubstitution :: String -> Type -> [String] -> Type -> WriterT Any (Either String) (Maybe Type)
buildTypeSubstitution name ty args = go (reverse args) M.empty
  where
  go :: [String] -> M.Map String Type -> Type -> WriterT Any (Either String) (Maybe Type)
  go [] m (TypeConstructor ctor) | name == ctor = tell (Any True) >> return (Just $ replaceTypeVars m ty)
  go (_:_) _ (TypeConstructor ctor) | name == ctor = lift . throwError $ "Partially applied type synonym " ++ name
  go (arg:args) m (TypeApp f x) = go args (M.insert arg x m) f
  go _ _ _ = return Nothing

everywhereButM' :: (Monad m, Data d) => (forall d. (Data d) => d -> WriterT Any m d) -> d -> m d
everywhereButM' f x = do
  (y, halt) <- runWriterT $ f x
  case getAny halt of
    True -> return y
    _ -> gmapM (everywhereButM' f) y

substituteTypeSynonym :: String -> [String] -> Type -> Type -> Either String Type
substituteTypeSynonym name args ty = everywhereButM' (mkM replace)
  where
  replace t = fmap (fromMaybe t) $ buildTypeSubstitution name ty args t
