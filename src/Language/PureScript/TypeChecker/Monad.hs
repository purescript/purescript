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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.TypeChecker.Monad where

import Data.Maybe
import qualified Data.Map as M

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.State
import Control.Monad.Unify
import Control.Monad.Writer.Strict
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

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
-- Temporarily bind a collection of names to types
--
withScopedTypeVars :: (Functor m, MonadState CheckState m) => ModuleName -> [(String, Kind)] -> m a -> m a
withScopedTypeVars mn ks = bindTypes (M.fromList (map (\(name, k) -> (Qualified (Just mn) (ProperName name), (k, ScopedTypeVar))) ks))

-- |
-- Temporarily make a collection of type class dictionaries available
--
withTypeClassDictionaries :: (MonadState CheckState m) => [TypeClassDictionaryInScope] -> m a -> m a
withTypeClassDictionaries entries action = do
  orig <- get
  let mentries = M.fromListWith (M.unionWith M.union) [ (mn, M.singleton className (M.singleton (tcdName entry) entry)) | entry@TypeClassDictionaryInScope{ tcdName = Qualified mn _, tcdClassName = className }  <- entries ]
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = M.unionWith (M.unionWith M.union) (typeClassDictionaries . checkEnv $ st) mentries } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = typeClassDictionaries . checkEnv $ orig } }
  return a

-- |
-- Get the currently available map of type class dictionaries
--
getTypeClassDictionaries :: (Functor m, MonadState CheckState m) => m (M.Map (Maybe ModuleName) (M.Map (Qualified ProperName) (M.Map (Qualified Ident) TypeClassDictionaryInScope)))
getTypeClassDictionaries = typeClassDictionaries . checkEnv <$> get

-- |
-- Lookup type class dictionaries in a module.
--
lookupTypeClassDictionaries :: (Functor m, MonadState CheckState m) => Maybe ModuleName -> m (M.Map (Qualified ProperName) (M.Map (Qualified Ident) TypeClassDictionaryInScope))
lookupTypeClassDictionaries mn = fromMaybe M.empty . M.lookup mn . typeClassDictionaries . checkEnv <$> get

-- |
-- Temporarily bind a collection of names to local variables
--
bindLocalVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(Ident, Type, NameVisibility)] -> m a -> m a
bindLocalVariables moduleName bindings =
  bindNames (M.fromList $ flip map bindings $ \(name, ty, visibility) -> ((moduleName, name), (ty, Private, visibility)))

-- |
-- Temporarily bind a collection of names to local type variables
--
bindLocalTypeVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(ProperName, Kind)] -> m a -> m a
bindLocalTypeVariables moduleName bindings =
  bindTypes (M.fromList $ flip map bindings $ \(pn, kind) -> (Qualified (Just moduleName) pn, (kind, LocalTypeVariable)))

-- |
-- Update the visibility of all names to Defined
--
makeBindingGroupVisible :: (Functor m, MonadState CheckState m) => m ()
makeBindingGroupVisible = modifyEnv $ \e -> e { names = M.map (\(ty, nk, _) -> (ty, nk, Defined)) (names e) }

-- | Update the visibility of all names to Defined in the scope of the provided action
withBindingGroupVisible :: (Functor m, MonadState CheckState m) => m a -> m a
withBindingGroupVisible action = preservingNames $ makeBindingGroupVisible >> action

-- | Perform an action while preserving the names from the @Environment@.
preservingNames :: (Functor m, MonadState CheckState m) => m a -> m a
preservingNames action = do
  orig <- gets (names . checkEnv)
  a <- action
  modifyEnv $ \e -> e { names = orig }
  return a

-- |
-- Lookup the type of a value by name in the @Environment@
--
lookupVariable :: (e ~ MultipleErrors, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m Type
lookupVariable currentModule (Qualified moduleName var) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, var) (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined var
    Just (ty, _, _) -> return ty

-- |
-- Lookup the visibility of a value by name in the @Environment@
--
getVisibility :: (e ~ MultipleErrors, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m NameVisibility
getVisibility currentModule (Qualified moduleName var) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, var) (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined var
    Just (_, _, vis) -> return vis

-- |
-- Assert that a name is visible
--
checkVisibility :: (e ~ MultipleErrors, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m ()
checkVisibility currentModule name@(Qualified _ var) = do
  vis <- getVisibility currentModule name
  case vis of
    Undefined -> throwError . errorMessage $ CycleInDeclaration var
    _ -> return ()

-- |
-- Lookup the kind of a type by name in the @Environment@
--
lookupTypeVariable :: (e ~ MultipleErrors, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified ProperName -> m Kind
lookupTypeVariable currentModule (Qualified moduleName name) = do
  env <- getEnv
  case M.lookup (Qualified (Just $ fromMaybe currentModule moduleName) name) (types env) of
    Nothing -> throwError . errorMessage $ UndefinedTypeVariable name
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
newtype Check a = Check { unCheck :: StateT CheckState (ExceptT MultipleErrors (Writer MultipleErrors)) a }
  deriving (Functor, Monad, Applicative, MonadState CheckState, MonadError MultipleErrors, MonadWriter MultipleErrors)

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
runCheck :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => Check a -> m (a, Environment)
runCheck = runCheck' initEnvironment

-- |
-- Run a computation in the Check monad, failing with an error, or succeeding with a return value and the final @Environment@.
--
runCheck' :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => Environment -> Check a -> m (a, Environment)
runCheck' env = interpretMultipleErrorsAndWarnings . unwrapCheckWithWarnings env
  where
  unwrapCheckWithWarnings :: Environment -> Check a -> (Either MultipleErrors (a, Environment), MultipleErrors)
  unwrapCheckWithWarnings e =
    (\(rc, w) -> (envCheck rc, w))
    . runWriter
    . runExceptT
    . flip runStateT (CheckState e 0 0 Nothing)
    . unCheck
  envCheck :: Either MultipleErrors (a, CheckState) -> Either MultipleErrors (a, Environment)
  envCheck rc = do
    (a, s) <- rc
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
liftUnify = liftUnifyWarnings (const id)

-- |
-- Run a computation in the substitution monad, generating a return value, the final substitution and updating warnings values.
--
liftUnifyWarnings :: (Partial t) => (Substitution t -> ErrorMessage -> ErrorMessage) -> UnifyT t Check a -> Check (a, Substitution t)
liftUnifyWarnings replace unify = do
  st <- get
  let ru = runUnify (defaultUnifyState { unifyNextVar = checkNextVar st }) unify
  ((a, ust), w) <- censor (const mempty) . listen $ ru
  modify $ \st' -> st' { checkNextVar = unifyNextVar ust }
  let uust = unifyCurrentSubstitution ust
  tell $ onErrorMessages (replace uust) w
  return (a, uust)
