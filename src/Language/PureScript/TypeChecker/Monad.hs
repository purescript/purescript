{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- |
-- Monads for type checking and type inference and associated data types
--
module Language.PureScript.TypeChecker.Monad where

import           Prelude.Compat

import           Control.Arrow (second)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.State
import           Control.Monad.Writer.Class (MonadWriter(..), listen, censor)

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Maybe
import qualified Data.Map as M

import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Kinds
import           Language.PureScript.Names
import           Language.PureScript.TypeClassDictionaries
import           Language.PureScript.Types

-- | A substitution of unification variables for types or kinds
data Substitution = Substitution
  { substType :: IM.IntMap Type -- ^ Type substitution
  , substKind :: IM.IntMap Kind -- ^ Kind substitution
  }

-- | An empty substitution
emptySubstitution :: Substitution
emptySubstitution = Substitution IM.empty IM.empty

data SkolemData = SkolemData
  { skolemUnifiedWith :: IS.IntSet
  -- ^ Any unification variables which this skolem was unified with
  , skolemLowBound :: Int
  -- ^ The least unification variable which this skolem variable
  -- is allowed to unify with
  , skolemHighBound :: Maybe Int
  -- ^ The highest unification variable which this skolem variable
  -- is allowed to unify with
  }

-- | Note that a skolem variable unified with a unification variable.
unifySkolem :: Int -> SkolemData -> SkolemData
unifySkolem u s = s { skolemUnifiedWith = IS.insert u (skolemUnifiedWith s) }

-- | Create an empty 'SkolemData' from the current unification variable.
newSkolemData :: Int -> SkolemData
newSkolemData u = SkolemData mempty u Nothing

-- | Mark a 'SkolemData' structure with the current unification variable at the
-- high bound.
closeSkolemData :: Int -> SkolemData -> SkolemData
closeSkolemData u s = s { skolemHighBound = Just u }

-- | State required for type checking
data CheckState = CheckState
  { checkEnv :: Environment
  -- ^ The current @Environment@
  , checkNextType :: Int
  -- ^ The next type unification variable
  , checkNextKind :: Int
  -- ^ The next kind unification variable
  , checkNextSkolem :: Int
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
  , checkSkolems :: IM.IntMap SkolemData
  -- ^ Whenever a skolem variable is unified with a
  -- unification variable, we log it here. When
  -- type-checking is complete, we use this map to check
  -- no skolems escaped their scope.
  }

-- | Generate a new skolem constant
newSkolemConstant
  :: MonadState CheckState m
  => (Int -> m a)
  -- ^ The action which defines the scope of the new skolem constant
  -> m a
newSkolemConstant scoped = do
  s <- gets checkNextSkolem
  low <- gets checkNextType
  modify $ \st -> st { checkNextSkolem = s + 1
                     , checkSkolems = IM.insert s (newSkolemData low) (checkSkolems st)
                     }
  a <- scoped s
  high <- gets checkNextType
  withSkolemData s (closeSkolemData high)
  return a

withSkolemData
  :: MonadState CheckState m
  => Int
  -> (SkolemData -> SkolemData)
  -> m ()
withSkolemData s f = modify $ \st -> st { checkSkolems = IM.adjust f s (checkSkolems st) }

-- | Create an empty @CheckState@
emptyCheckState :: Environment -> CheckState
emptyCheckState env = CheckState env 0 0 0 Nothing emptySubstitution [] mempty

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
runCheck = runCheck' initEnvironment

-- | Run a computation in the typechecking monad, failing with an error, or succeeding with a return value and the final @Environment@.
runCheck' :: (Functor m) => Environment -> StateT CheckState m a -> m (a, Environment)
runCheck' env check = second checkEnv <$> runStateT check (emptyCheckState env)

-- | Make an assertion, failing with an error message
guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

-- | Run a computation in the substitution monad, generating a return value and the final substitution.
liftUnify ::
  (MonadState CheckState m, MonadWriter MultipleErrors m, MonadError MultipleErrors m) =>
  m a ->
  m (a, Substitution)
liftUnify = liftUnifyWarnings (const id)

-- | Run a computation in the substitution monad, generating a return value, the final substitution and updating warnings values.
liftUnifyWarnings ::
  (MonadState CheckState m, MonadWriter MultipleErrors m, MonadError MultipleErrors m) =>
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
