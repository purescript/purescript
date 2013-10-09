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

import Control.Arrow ((***))

import qualified Data.Map as M

data NameKind = Value | Extern deriving Show

data TypeDeclarationKind = Data | TypeSynonym deriving Show

data Environment = Environment
  { names :: M.Map String (PolyType, NameKind)
  , types :: M.Map String (Kind, TypeDeclarationKind)
  , dataConstructors :: M.Map String PolyType
  , typeSynonyms :: M.Map String ([String], Type)
  }

emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty M.empty M.empty M.empty

newtype Check a = Check { unCheck :: StateT (Environment, Int) (Either String) a } deriving (Functor, Monad, Applicative, MonadPlus, MonadState (Environment, Int), MonadError String)

getEnv :: Check Environment
getEnv = fmap fst get

putEnv :: Environment -> Check ()
putEnv env = fmap (const env *** id) get >>= put

fresh :: Check Int
fresh = do
  (env, n) <- get
  put (env, n + 1)
  return n

check :: Check a -> Either String (a, Environment)
check = fmap (id *** fst) . flip runStateT (emptyEnvironment, 0) . unCheck

guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)
