-----------------------------------------------------------------------------
--
-- Module      :  PureScript.TypeChecker.Monad
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

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module PureScript.TypeChecker.Monad where

import PureScript.Types
import PureScript.Kinds

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as M

data Environment = Environment
  { names :: M.Map String Type
  , types :: M.Map String Kind
  } deriving (Show)

emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty M.empty

newtype Check a = Check { unCheck :: StateT Environment (Either String) a } deriving (Functor, Monad, Applicative, MonadPlus, MonadState Environment, MonadError String)

check :: Check a -> Either String a
check = flip evalStateT emptyEnvironment . unCheck

guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)
