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
import Language.PureScript.Names
import Language.PureScript.Declarations (canonicalizeDictionary)
import Language.PureScript.Environment
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Options
import Language.PureScript.Errors

import Data.Maybe

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Unify

import qualified Data.Map as M

-- |
-- Temporarily bind a collection of names to values
--
bindNames :: (MonadState CheckState m) => M.Map (ModuleName, Ident) (Type, NameKind, NameVisibility) -> m a -> m a
bindNames newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { names = newNames `M.union` (names . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { names = names . checkEnv $ orig } }
  return a

-- |
-- Temporarily bind a collection of names to types
--
bindTypes :: (MonadState CheckState m) => M.Map (Qualified ProperName) (Kind, TypeKind) -> m a -> m a
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
  let mentries = M.fromList [ ((canonicalizeDictionary entry, mn), entry) | entry@TypeClassDictionaryInScope{ tcdName = Qualified mn _ }  <- entries ]
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = (typeClassDictionaries . checkEnv $ st) `M.union` mentries } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = typeClassDictionaries . checkEnv $ orig } }
  return a

-- |
-- Get the currently available list of type class dictionaries
--
getTypeClassDictionaries :: (Functor m, MonadState CheckState m) => m [TypeClassDictionaryInScope]
getTypeClassDictionaries = M.elems . typeClassDictionaries . checkEnv <$> get

-- |
-- Temporarily bind a collection of names to local variables
--
bindLocalVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(Ident, Type, NameVisibility)] -> m a -> m a
bindLocalVariables moduleName bindings =
  bindNames (M.fromList $ flip map bindings $ \(name, ty, visibility) -> ((moduleName, name), (ty, LocalVariable, visibility)))

-- |
-- Temporarily bind a collection of names to local type variables
--
bindLocalTypeVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(ProperName, Kind)] -> m a -> m a
bindLocalTypeVariables moduleName bindings =
  bindTypes (M.fromList $ flip map bindings $ \(pn, kind) -> (Qualified (Just moduleName) pn, (kind, LocalTypeVariable)))

-- |
-- Update the visibility of all names to Defined
--
makeBindingGroupVisible :: (Functor m, MonadState CheckState m) => m a -> m a
makeBindingGroupVisible action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { names = M.map (\(ty, nk, _) -> (ty, nk, Defined)) (names . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { names = names . checkEnv $ orig } }
  return a

-- |
-- Lookup the type of a value by name in the @Environment@
--
lookupVariable :: (Error e, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m Type
lookupVariable currentModule (Qualified moduleName var) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, var) (names env) of
    Nothing -> throwError . strMsg $ show var ++ " is undefined"
    Just (ty, _, _) -> return ty

-- |
-- Lookup the visibility of a value by name in the @Environment@
--
getVisibility :: (Error e, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m NameVisibility
getVisibility currentModule (Qualified moduleName var) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, var) (names env) of
    Nothing -> throwError . strMsg $ show var ++ " is undefined"
    Just (_, _, vis) -> return vis

-- |
-- Assert that a name is visible
--
checkVisibility :: (Error e, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m ()
checkVisibility currentModule name@(Qualified _ var) = do
  vis <- getVisibility currentModule name
  case vis of
    Undefined -> throwError . strMsg $ show var ++ " may not be defined in the current scope."
    _ -> return ()

-- |
-- Lookup the kind of a type by name in the @Environment@
--
lookupTypeVariable :: (Error e, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified ProperName -> m Kind
lookupTypeVariable currentModule (Qualified moduleName name) = do
  env <- getEnv
  case M.lookup (Qualified (Just $ fromMaybe currentModule moduleName) name) (types env) of
    Nothing -> throwError . strMsg $ "Type variable " ++ show name ++ " is undefined"
    Just (k, _) -> return k

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
newtype Check a = Check { unCheck :: StateT CheckState (Either ErrorStack) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadPlus, MonadState CheckState, MonadError ErrorStack)

-- |
-- Get the current @Environment@
--
getEnv :: (Functor m, MonadState CheckState m) => m Environment
getEnv = checkEnv <$> get

-- |
-- Update the @Environment@
--
putEnv :: (MonadState CheckState m) => Environment -> m ()
putEnv env = modify (\s -> s { checkEnv = env })

-- |
-- Modify the @Environment@
--
modifyEnv :: (MonadState CheckState m) => (Environment -> Environment) -> m ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

-- |
-- Run a computation in the Check monad, starting with an empty @Environment@
--
runCheck :: Options mode -> Check a -> Either String (a, Environment)
runCheck opts = runCheck' opts initEnvironment

-- |
-- Run a computation in the Check monad, failing with an error, or succeeding with a return value and the final @Environment@.
--
runCheck' :: Options mode -> Environment -> Check a -> Either String (a, Environment)
runCheck' opts env c = stringifyErrorStack (optionsVerboseErrors opts) $ do
  (a, s) <- flip runStateT (CheckState env 0 0 Nothing) $ unCheck c
  return (a, checkEnv s)

-- |
-- Make an assertion, failing with an error message
--
guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

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
liftCheck = UnifyT . lift

-- |
-- Run a computation in the substitution monad, generating a return value and the final substitution.
--
liftUnify :: (Partial t) => UnifyT t Check a -> Check (a, Substitution t)
liftUnify unify = do
  st <- get
  (a, ust) <- runUnify (defaultUnifyState { unifyNextVar = checkNextVar st }) unify
  modify $ \st' -> st' { checkNextVar = unifyNextVar ust }
  return (a, unifyCurrentSubstitution ust)
