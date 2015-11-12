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

module Language.PureScript.TypeChecker.Monad where

import Prelude ()
import Prelude.Compat

import Data.Maybe
import qualified Data.Map as M

import Control.Arrow (second)
import Control.Monad.State
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..), listen, censor)

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

-- | A substitution of unification variables for types or kinds
data Substitution = Substitution
  { substType :: M.Map Int Type -- ^ Type substitution
  , substKind :: M.Map Int Kind -- ^ Kind substitution
  }

-- | An empty substitution
emptySubstitution :: Substitution
emptySubstitution = Substitution M.empty M.empty

-- | State required for type checking
data CheckState = CheckState
  { checkEnv :: Environment                 -- ^ The current @Environment@
  , checkNextType :: Int                    -- ^ The next type unification variable
  , checkNextKind :: Int                    -- ^ The next kind unification variable
  , checkNextSkolem :: Int                  -- ^ The next skolem variable
  , checkNextSkolemScope :: Int             -- ^ The next skolem scope constant
  , checkNextDictName :: Int                -- ^ The next type class dictionary name
  , checkCurrentModule :: Maybe ModuleName  -- ^ The current module
  , checkSubstitution :: Substitution       -- ^ The current substitution
  }

-- | Create an empty @CheckState@
emptyCheckState :: Environment -> CheckState
emptyCheckState env = CheckState env 0 0 0 0 0 Nothing emptySubstitution

-- | Unification variables
type Unknown = Int

-- | Temporarily bind a collection of names to values
bindNames :: (MonadState CheckState m) => M.Map (ModuleName, Ident) (Type, NameKind, NameVisibility) -> m a -> m a
bindNames newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { names = newNames `M.union` (names . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { names = names . checkEnv $ orig } }
  return a

-- | Temporarily bind a collection of names to types
bindTypes :: (MonadState CheckState m) => M.Map (Qualified ProperName) (Kind, TypeKind) -> m a -> m a
bindTypes newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { types = newNames `M.union` (types . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { types = types . checkEnv $ orig } }
  return a

-- | Temporarily bind a collection of names to types
withScopedTypeVars :: (Functor m, Applicative m, MonadState CheckState m, MonadWriter MultipleErrors m) => ModuleName -> [(String, Kind)] -> m a -> m a
withScopedTypeVars mn ks ma = do
  orig <- get
  forM_ ks $ \(name, _) ->
    when (Qualified (Just mn) (ProperName name) `M.member` types (checkEnv orig)) $
      tell . errorMessage $ ShadowedTypeVar name
  bindTypes (M.fromList (map (\(name, k) -> (Qualified (Just mn) (ProperName name), (k, ScopedTypeVar))) ks)) ma

-- | Temporarily make a collection of type class dictionaries available
withTypeClassDictionaries :: (MonadState CheckState m) => [TypeClassDictionaryInScope] -> m a -> m a
withTypeClassDictionaries entries action = do
  orig <- get
  let mentries = M.fromListWith (M.unionWith M.union) [ (mn, M.singleton className (M.singleton (tcdName entry) entry)) | entry@TypeClassDictionaryInScope{ tcdName = Qualified mn _, tcdClassName = className }  <- entries ]
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = M.unionWith (M.unionWith M.union) (typeClassDictionaries . checkEnv $ st) mentries } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = typeClassDictionaries . checkEnv $ orig } }
  return a

-- | Get the currently available map of type class dictionaries
getTypeClassDictionaries ::
  (Functor m, MonadState CheckState m) =>
  m (M.Map (Maybe ModuleName) (M.Map (Qualified ProperName) (M.Map (Qualified Ident) TypeClassDictionaryInScope)))
getTypeClassDictionaries = typeClassDictionaries . checkEnv <$> get

-- | Lookup type class dictionaries in a module.
lookupTypeClassDictionaries ::
  (Functor m, MonadState CheckState m) =>
  Maybe ModuleName ->
  m (M.Map (Qualified ProperName) (M.Map (Qualified Ident) TypeClassDictionaryInScope))
lookupTypeClassDictionaries mn = fromMaybe M.empty . M.lookup mn . typeClassDictionaries . checkEnv <$> get

-- | Temporarily bind a collection of names to local variables
bindLocalVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(Ident, Type, NameVisibility)] -> m a -> m a
bindLocalVariables moduleName bindings =
  bindNames (M.fromList $ flip map bindings $ \(name, ty, visibility) -> ((moduleName, name), (ty, Private, visibility)))

-- | Temporarily bind a collection of names to local type variables
bindLocalTypeVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(ProperName, Kind)] -> m a -> m a
bindLocalTypeVariables moduleName bindings =
  bindTypes (M.fromList $ flip map bindings $ \(pn, kind) -> (Qualified (Just moduleName) pn, (kind, LocalTypeVariable)))

-- | Update the visibility of all names to Defined
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

-- | Lookup the type of a value by name in the @Environment@
lookupVariable :: (e ~ MultipleErrors, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m Type
lookupVariable currentModule (Qualified moduleName var) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, var) (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined var
    Just (ty, _, _) -> return ty

-- | Lookup the visibility of a value by name in the @Environment@
getVisibility :: (e ~ MultipleErrors, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m NameVisibility
getVisibility currentModule (Qualified moduleName var) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, var) (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined var
    Just (_, _, vis) -> return vis

-- | Assert that a name is visible
checkVisibility :: (e ~ MultipleErrors, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified Ident -> m ()
checkVisibility currentModule name@(Qualified _ var) = do
  vis <- getVisibility currentModule name
  case vis of
    Undefined -> throwError . errorMessage $ CycleInDeclaration var
    _ -> return ()

-- | Lookup the kind of a type by name in the @Environment@
lookupTypeVariable :: (e ~ MultipleErrors, Functor m, MonadState CheckState m, MonadError e m) => ModuleName -> Qualified ProperName -> m Kind
lookupTypeVariable currentModule (Qualified moduleName name) = do
  env <- getEnv
  case M.lookup (Qualified (Just $ fromMaybe currentModule moduleName) name) (types env) of
    Nothing -> throwError . errorMessage $ UndefinedTypeVariable name
    Just (k, _) -> return k

-- | Get the current @Environment@
getEnv :: (Functor m, MonadState CheckState m) => m Environment
getEnv = checkEnv <$> get

-- | Update the @Environment@
putEnv :: (MonadState CheckState m) => Environment -> m ()
putEnv env = modify (\s -> s { checkEnv = env })

-- | Modify the @Environment@
modifyEnv :: (MonadState CheckState m) => (Environment -> Environment) -> m ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

-- | Run a computation in the typechecking monad, starting with an empty @Environment@
runCheck :: (Functor m) => StateT CheckState m a -> m (a, Environment)
runCheck = runCheck' initEnvironment

-- | Run a computation in the typechecking monad, failing with an error, or succeeding with a return value and the final @Environment@.
runCheck' :: (Functor m) => Environment -> StateT CheckState m a -> m (a, Environment)
runCheck' env check = fmap (second checkEnv) $ runStateT check (emptyCheckState env)

-- | Make an assertion, failing with an error message
guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

-- | Generate new type class dictionary name
freshDictionaryName :: (Functor m, MonadState CheckState m) => m Int
freshDictionaryName = do
  n <- checkNextDictName <$> get
  modify $ \s -> s { checkNextDictName = succ (checkNextDictName s) }
  return n

-- | Run a computation in the substitution monad, generating a return value and the final substitution.
liftUnify ::
  (Functor m, MonadState CheckState m, MonadWriter MultipleErrors m, MonadError MultipleErrors m) =>
  m a ->
  m (a, Substitution)
liftUnify = liftUnifyWarnings (const id)

-- | Run a computation in the substitution monad, generating a return value, the final substitution and updating warnings values.
liftUnifyWarnings ::
  (Functor m, MonadState CheckState m, MonadWriter MultipleErrors m, MonadError MultipleErrors m) =>
  (Substitution -> ErrorMessage -> ErrorMessage) ->
  m a ->
  m (a, Substitution)
liftUnifyWarnings replace ma = do
  orig <- get
  modify $ \st -> st { checkSubstitution = emptySubstitution }
  (a, w) <- reflectErrors . censor (const mempty) . reifyErrors . listen $ ma
  subst <- gets checkSubstitution
  tell . onErrorMessages (replace subst) $ w
  modify $ \st -> st { checkSubstitution = checkSubstitution orig }
  return (a, subst)
