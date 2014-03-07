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
-- Monads for type checking and type inference and associated data types
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, RankNTypes,
    MultiParamTypeClasses, FlexibleContexts #-}

module Language.PureScript.TypeChecker.Monad where

import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Prim

import Data.Maybe

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Unify
import Control.Arrow (first)

import qualified Data.Map as M

-- |
-- The @Environment@ defines all values and types which are currently in scope:
--
data Environment = Environment {
  -- |
  -- Value names currently in scope
  --
    names :: M.Map (ModuleName, Ident) (Type, NameKind)
  -- |
  -- Type names currently in scope
  --
  , types :: M.Map (Qualified ProperName) Kind
  -- |
  -- Data constructors currently in scope, along with their associated data type constructors
  --
  , dataConstructors :: M.Map (Qualified ProperName) (ProperName, Type)
  -- |
  -- Type synonyms currently in scope
  --
  , typeSynonyms :: M.Map (Qualified ProperName) ([String], Type)
  -- |
  -- Available type class dictionaries
  --
  , typeClassDictionaries :: [TypeClassDictionaryInScope]
  } deriving (Show)

-- |
-- The initial environment with no values and only the default javascript types defined
--
initEnvironment :: Environment
initEnvironment = Environment M.empty primTypes M.empty M.empty []

-- |
-- Temporarily bind a collection of names to values
--
bindNames :: (MonadState CheckState m) => M.Map (ModuleName, Ident) (Type, NameKind) -> m a -> m a
bindNames newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { names = newNames `M.union` (names . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { names = names . checkEnv $ orig } }
  return a

-- |
-- Temporarily bind a collection of names to types
--
bindTypes :: (MonadState CheckState m) => M.Map (Qualified ProperName) Kind -> m a -> m a
bindTypes newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { types = newNames `M.union` (types . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { types = types . checkEnv $ orig } }
  return a

-- |
-- Temporarily make a collection of type class dictionaries available
--
withTypeClassDictionaries :: (MonadState CheckState m) => [TypeClassDictionaryInScope] -> m a -> m a
withTypeClassDictionaries entries action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = entries ++ (typeClassDictionaries . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = typeClassDictionaries . checkEnv $ orig } }
  return a

-- |
-- Get the currently available list of type class dictionaries
--
getTypeClassDictionaries :: (Functor m, MonadState CheckState m) => m [TypeClassDictionaryInScope]
getTypeClassDictionaries = typeClassDictionaries . checkEnv <$> get

-- |
-- Temporarily bind a collection of names to local variables
--
bindLocalVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(Ident, Type)] -> m a -> m a
bindLocalVariables moduleName bindings =
  bindNames (M.fromList $ flip map bindings $ \(name, ty) -> ((moduleName, name), (ty, LocalVariable)))

-- |
-- Temporarily bind a collection of names to local type variables
--
bindLocalTypeVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(ProperName, Kind)] -> m a -> m a
bindLocalTypeVariables moduleName bindings =
  bindTypes (M.fromList $ flip map bindings $ first $ Qualified (Just moduleName))

-- |
-- Lookup the type of a value by name in the @Environment@
--
lookupVariable :: (Functor m, MonadState CheckState m, MonadError String m) => ModuleName -> Qualified Ident -> m Type
lookupVariable currentModule (Qualified moduleName var) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, var) (names env) of
    Nothing -> throwError $ show var ++ " is undefined"
    Just (ty, _) -> return ty

-- |
-- Lookup the kind of a type by name in the @Environment@
--
lookupTypeVariable :: (Functor m, MonadState CheckState m, MonadError String m) => ModuleName -> Qualified ProperName -> m Kind
lookupTypeVariable currentModule (Qualified moduleName name) = do
  env <- getEnv
  case M.lookup (Qualified (Just $ fromMaybe currentModule moduleName) name) (types env) of
    Nothing -> throwError $ "Type variable " ++ show name ++ " is undefined"
    Just k -> return k

-- |
-- State required for type checking:
--
data CheckState = CheckState {
  -- |
  -- The current @Environment@
  --
    checkEnv :: Environment
  -- |
  -- The next fresh unification variable name
  --
  , checkNextVar :: Int
  -- |
  -- The next type class dictionary name
  --
  , checkNextDictName :: Int
  -- |
  -- The current module
  --
  , checkCurrentModule :: Maybe ModuleName
  }

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
newtype Check a = Check { unCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Monad, Applicative, MonadPlus, MonadState CheckState, MonadError String)

-- |
-- Get the current @Environment@
--
getEnv :: (Functor m, MonadState CheckState m) => m Environment
getEnv = checkEnv <$> get

-- |
-- Update the @Environment#
--
putEnv :: (MonadState CheckState m) => Environment -> m ()
putEnv env = modify (\s -> s { checkEnv = env })

-- |
-- Modify the @Environment@
--
modifyEnv :: (MonadState CheckState m) => (Environment -> Environment) -> m ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

-- |
-- Run a computation in the Check monad, failing with an error, or succeeding with a return value and the final @Environment@.
--
runCheck :: Check a -> Either String (a, Environment)
runCheck c = do
  (a, s) <- flip runStateT (CheckState initEnvironment 0 0 Nothing) $ unCheck c
  return (a, checkEnv s)

-- |
-- Make an assertion, failing with an error message
--
guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

-- |
-- Rethrow an error with a more detailed error message in the case of failure
--
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)

-- |
-- Generate new type class dictionary name
--
freshDictionaryName :: Check Int
freshDictionaryName = do
  n <- checkNextDictName <$> get
  modify $ \s -> s { checkNextDictName = succ (checkNextDictName s) }
  return n

-- |
-- Lift a computation in the @Check@ monad into the substitution monad.
--
liftCheck :: Check a -> UnifyT t Check a
liftCheck = UnifyT . lift . lift

-- |
-- Run a computation in the substitution monad, generating a return value and the final substitution.
--
liftUnify :: (Partial t) => UnifyT t Check a -> Check (a, Substitution t)
liftUnify unify = do
  st <- get
  e <- runUnify (defaultUnifyState { unifyNextVar = checkNextVar st }) unify
  case e of
    Left err -> throwError err
    Right (a, ust) -> do
      modify $ \st' -> st' { checkNextVar = unifyNextVar ust }
      return (a, unifyCurrentSubstitution ust)
