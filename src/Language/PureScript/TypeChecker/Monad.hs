-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Monad
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

module Language.PureScript.TypeChecker.Monad where

import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import Control.Arrow ((***), first, second)

import qualified Data.Map as M

data NameKind = Value | Extern | Alias ModulePath Ident deriving Show

data TypeDeclarationKind = Data | ExternData | TypeSynonym deriving Show

data Environment = Environment
  { names :: M.Map (ModulePath, Ident) (Type, NameKind)
  , types :: M.Map (ModulePath, ProperName) (Kind, TypeDeclarationKind)
  , dataConstructors :: M.Map (ModulePath, ProperName) Type
  , typeSynonyms :: M.Map (ModulePath, ProperName) ([String], Type)
  , members :: M.Map (ModulePath, Ident) String
  } deriving (Show)

emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty M.empty M.empty M.empty M.empty

data CheckState = CheckState { checkEnv :: Environment
                             , checkNextVar :: Int
                             , checkModulePath :: ModulePath
                             } deriving (Show)

newtype Check a = Check { unCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Monad, Applicative, MonadPlus, MonadState CheckState, MonadError String)

getEnv :: Check Environment
getEnv = fmap checkEnv get

putEnv :: Environment -> Check ()
putEnv env = modify (\s -> s { checkEnv = env })

modifyEnv :: (Environment -> Environment) -> Check ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

fresh :: Check Int
fresh = do
  st <- get
  put $ st { checkNextVar = checkNextVar st + 1 }
  return $ checkNextVar st

check :: Check a -> Either String (a, Environment)
check = fmap (second checkEnv) . flip runStateT (CheckState emptyEnvironment 0 global) . unCheck

guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)

withModule :: ProperName -> Check a -> Check a
withModule name act = do
  original <- checkModulePath `fmap` get
  modify $ \s -> s { checkModulePath = subModule (checkModulePath s) name }
  a <- act
  modify $ \s -> s { checkModulePath = original }
  return a
