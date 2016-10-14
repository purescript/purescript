{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- |
-- Monads for type checking and type inference and associated data types
--
module Language.PureScript.TypeChecker.Monad where

import Prelude.Compat

import Control.Arrow (second)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State
import Control.Monad.Writer.Class (MonadWriter(..), censor)

import Data.Maybe
import qualified Data.Map as M

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
  { checkEnv :: Environment
  -- ^ The current @Environment@
  , checkNextType :: Int
  -- ^ The next type unification variable
  , checkNextKind :: Int
  -- ^ The next kind unification variable
  , checkNextSkolem :: Int
  -- ^ The next skolem variable
  , checkNextSkolemScope :: Int
  -- ^ The next skolem scope constant
  , checkCurrentModule :: Maybe ModuleName
  -- ^ The current module
  , checkSubstitution :: Substitution
  -- ^ The current substitution
  , checkHints :: [ErrorMessageHint]
  -- ^ The current error message hint stack.
  -- This goes into state, rather than using 'rethrow',
  -- since this way, we can provide good error messages
  -- during instance resolution.
  }

-- | Create an empty @CheckState@
emptyCheckState :: Environment -> CheckState
emptyCheckState env = CheckState env 0 0 0 0 Nothing emptySubstitution []

-- | Unification variables
type Unknown = Int

-- | Temporarily bind a collection of names to values
bindNames
  :: MonadState CheckState m
  => M.Map (Qualified Ident) (Type, NameKind, NameVisibility)
  -> m a
  -> m a
bindNames newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { names = newNames `M.union` (names . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { names = names . checkEnv $ orig } }
  return a

-- | Temporarily bind a collection of names to types
bindTypes
  :: MonadState CheckState m
  => M.Map (Qualified (ProperName 'TypeName)) (Kind, TypeKind)
  -> m a
  -> m a
bindTypes newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { types = newNames `M.union` (types . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { types = types . checkEnv $ orig } }
  return a

-- | Temporarily bind a collection of names to types
withScopedTypeVars
  :: (MonadState CheckState m, MonadWriter MultipleErrors m)
  => ModuleName
  -> [(String, Kind)]
  -> m a
  -> m a
withScopedTypeVars mn ks ma = do
  orig <- get
  forM_ ks $ \(name, _) ->
    when (Qualified (Just mn) (ProperName name) `M.member` types (checkEnv orig)) $
      tell . errorMessage $ ShadowedTypeVar name
  bindTypes (M.fromList (map (\(name, k) -> (Qualified (Just mn) (ProperName name), (k, ScopedTypeVar))) ks)) ma

withErrorMessageHint
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => ErrorMessageHint
  -> m a
  -> m a
withErrorMessageHint hint action = do
  orig <- get
  modify $ \st -> st { checkHints = hint : checkHints st }
  -- Need to use 'rethrow' anyway, since we have to handle regular errors
  a <- rethrow (addHint hint) action
  modify $ \st -> st { checkHints = checkHints orig }
  return a

rethrowWithPositionTC
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => SourceSpan
  -> m a
  -> m a
rethrowWithPositionTC pos = withErrorMessageHint (PositionedError pos)

warnAndRethrowWithPositionTC
  :: (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => SourceSpan
  -> m a
  -> m a
warnAndRethrowWithPositionTC pos = rethrowWithPositionTC pos . warnWithPosition pos

-- | Temporarily make a collection of type class dictionaries available
withTypeClassDictionaries
  :: MonadState CheckState m
  => [TypeClassDictionaryInScope]
  -> m a
  -> m a
withTypeClassDictionaries entries action = do
  orig <- get
  let mentries = M.fromListWith (M.unionWith M.union) [ (mn, M.singleton className (M.singleton (tcdName entry) entry)) | entry@TypeClassDictionaryInScope{ tcdName = Qualified mn _, tcdClassName = className }  <- entries ]
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = M.unionWith (M.unionWith M.union) (typeClassDictionaries . checkEnv $ st) mentries } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = typeClassDictionaries . checkEnv $ orig } }
  return a

-- | Get the currently available map of type class dictionaries
getTypeClassDictionaries
  :: (MonadState CheckState m)
  => m (M.Map (Maybe ModuleName) (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) TypeClassDictionaryInScope)))
getTypeClassDictionaries = typeClassDictionaries . checkEnv <$> get

-- | Lookup type class dictionaries in a module.
lookupTypeClassDictionaries
  :: (MonadState CheckState m)
  => Maybe ModuleName
  -> m (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) TypeClassDictionaryInScope))
lookupTypeClassDictionaries mn = fromMaybe M.empty . M.lookup mn . typeClassDictionaries . checkEnv <$> get

-- | Temporarily bind a collection of names to local variables
bindLocalVariables
  :: (MonadState CheckState m)
  => [(Ident, Type, NameVisibility)]
  -> m a
  -> m a
bindLocalVariables bindings =
  bindNames (M.fromList $ flip map bindings $ \(name, ty, visibility) -> (Qualified Nothing name, (ty, Private, visibility)))

-- | Temporarily bind a collection of names to local type variables
bindLocalTypeVariables
  :: (MonadState CheckState m)
  => ModuleName
  -> [(ProperName 'TypeName, Kind)]
  -> m a
  -> m a
bindLocalTypeVariables moduleName bindings =
  bindTypes (M.fromList $ flip map bindings $ \(pn, kind) -> (Qualified (Just moduleName) pn, (kind, LocalTypeVariable)))

-- | Update the visibility of all names to Defined
makeBindingGroupVisible :: (MonadState CheckState m) => m ()
makeBindingGroupVisible = modifyEnv $ \e -> e { names = M.map (\(ty, nk, _) -> (ty, nk, Defined)) (names e) }

-- | Update the visibility of all names to Defined in the scope of the provided action
withBindingGroupVisible :: (MonadState CheckState m) => m a -> m a
withBindingGroupVisible action = preservingNames $ makeBindingGroupVisible >> action

-- | Perform an action while preserving the names from the @Environment@.
preservingNames :: (MonadState CheckState m) => m a -> m a
preservingNames action = do
  orig <- gets (names . checkEnv)
  a <- action
  modifyEnv $ \e -> e { names = orig }
  return a

-- | Lookup the type of a value by name in the @Environment@
lookupVariable
  :: (e ~ MultipleErrors, MonadState CheckState m, MonadError e m)
  => Qualified Ident
  -> m Type
lookupVariable qual = do
  env <- getEnv
  case M.lookup qual (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined (disqualify qual)
    Just (ty, _, _) -> return ty

-- | Lookup the visibility of a value by name in the @Environment@
getVisibility
  :: (e ~ MultipleErrors, MonadState CheckState m, MonadError e m)
  => Qualified Ident
  -> m NameVisibility
getVisibility qual = do
  env <- getEnv
  case M.lookup qual (names env) of
    Nothing -> throwError . errorMessage $ NameIsUndefined (disqualify qual)
    Just (_, _, vis) -> return vis

-- | Assert that a name is visible
checkVisibility
  :: (e ~ MultipleErrors, MonadState CheckState m, MonadError e m)
  => Qualified Ident
  -> m ()
checkVisibility name@(Qualified _ var) = do
  vis <- getVisibility name
  case vis of
    Undefined -> throwError . errorMessage $ CycleInDeclaration var
    _ -> return ()

-- | Lookup the kind of a type by name in the @Environment@
lookupTypeVariable
  :: (e ~ MultipleErrors, MonadState CheckState m, MonadError e m)
  => ModuleName
  -> Qualified (ProperName 'TypeName)
  -> m Kind
lookupTypeVariable currentModule (Qualified moduleName name) = do
  env <- getEnv
  case M.lookup (Qualified (Just $ fromMaybe currentModule moduleName) name) (types env) of
    Nothing -> throwError . errorMessage $ UndefinedTypeVariable name
    Just (k, _) -> return k

-- | Get the current @Environment@
getEnv :: (MonadState CheckState m) => m Environment
getEnv = checkEnv <$> get

-- | Get locally-bound names in context, to create an error message.
getLocalContext :: MonadState CheckState m => m Context
getLocalContext = do
  env <- getEnv
  return [ (ident, ty') | (Qualified Nothing ident@Ident{}, (ty', _, Defined)) <- M.toList (names env) ]

-- | Update the @Environment@
putEnv :: (MonadState CheckState m) => Environment -> m ()
putEnv env = modify (\s -> s { checkEnv = env })

-- | Modify the @Environment@
modifyEnv :: (MonadState CheckState m) => (Environment -> Environment) -> m ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

-- | Run a computation in the typechecking monad, starting with an empty @Environment@
runCheck :: (Functor m) => StateT CheckState m a -> m (a, Environment)
runCheck = runCheck' (emptyCheckState initEnvironment)

-- | Run a computation in the typechecking monad, failing with an error, or succeeding with a return value and the final @Environment@.
runCheck' :: (Functor m) => CheckState -> StateT CheckState m a -> m (a, Environment)
runCheck' st check = second checkEnv <$> runStateT check st

-- | Make an assertion, failing with an error message
guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

-- | Run a computation in the substitution monad, generating a return value and the final substitution.
captureSubstitution
  :: MonadState CheckState m
  => m a
  -> m (a, Substitution)
captureSubstitution = capturingSubstitution (,)

capturingSubstitution
  :: MonadState CheckState m
  => (a -> Substitution -> b)
  -> m a
  -> m b
capturingSubstitution f ma = do
  a <- ma
  subst <- gets checkSubstitution
  return (f a subst)

withFreshSubstitution
  :: MonadState CheckState m
  => m a
  -> m a
withFreshSubstitution ma = do
  orig <- get
  modify $ \st -> st { checkSubstitution = emptySubstitution }
  a <- ma
  modify $ \st -> st { checkSubstitution = checkSubstitution orig }
  return a

withoutWarnings
  :: MonadWriter w m
  => m a
  -> m (a, w)
withoutWarnings = censor (const mempty) . listen
