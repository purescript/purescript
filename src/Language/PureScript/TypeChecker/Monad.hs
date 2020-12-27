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

import Data.List (intercalate)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, isPrefixOf, unpack)
import qualified Data.List.NonEmpty as NEL

import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Pretty.Types
import Language.PureScript.Pretty.Values
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types
import Text.PrettyPrint.Boxes (render)

newtype UnkLevel = UnkLevel (NEL.NonEmpty Unknown)
  deriving (Eq, Show)

-- This instance differs from the NEL instance in that longer but otherwise
-- equal paths are LT rather than GT. An extended path puts it *before* its root.
instance Ord UnkLevel where
  compare (UnkLevel a) (UnkLevel b) =
    go (NEL.toList a) (NEL.toList b)
    where
    go [] [] = EQ
    go _  [] = LT
    go [] _  = GT
    go (x:xs) (y:ys) =
      compare x y <> go xs ys

-- | A substitution of unification variables for types.
data Substitution = Substitution
  { substType :: M.Map Int SourceType
  -- ^ Type substitution
  , substUnsolved :: M.Map Int (UnkLevel, SourceType)
  -- ^ Unsolved unification variables with their level (scope ordering) and kind
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
  , checkNextSkolem :: Int
  -- ^ The next skolem variable
  , checkNextSkolemScope :: Int
  -- ^ The next skolem scope constant
  , checkCurrentModule :: Maybe ModuleName
  -- ^ The current module
  , checkCurrentModuleImports ::
      [ ( SourceAnn
        , ModuleName
        , ImportDeclarationType
        , Maybe ModuleName
        , M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
        )
      ]
  -- ^ The current module imports and their exported types.
  -- Newtype constructors have to be in scope for some Coercible constraints to
  -- be solvable, so we need to know which constructors are imported and whether
  -- they are actually defined in or re-exported from the imported modules.
  , checkSubstitution :: Substitution
  -- ^ The current substitution
  , checkHints :: [ErrorMessageHint]
  -- ^ The current error message hint stack.
  -- This goes into state, rather than using 'rethrow',
  -- since this way, we can provide good error messages
  -- during instance resolution.
  , checkConstructorImportsForCoercible :: S.Set (ModuleName, Qualified (ProperName 'ConstructorName))
  -- ^ Newtype constructors imports required to solve Coercible constraints.
  -- We have to keep track of them so that we don't emit unused import warnings.
  }

-- | Create an empty @CheckState@
emptyCheckState :: Environment -> CheckState
emptyCheckState env = CheckState env 0 0 0 Nothing [] emptySubstitution [] mempty

-- | Unification variables
type Unknown = Int

-- | Temporarily bind a collection of names to values
bindNames
  :: MonadState CheckState m
  => M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
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
  => M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
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
  -> [(Text, SourceType)]
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

-- | These hints are added at the front, so the most nested hint occurs
-- at the front, but the simplifier assumes the reverse order.
getHints :: MonadState CheckState m => m [ErrorMessageHint]
getHints = gets (reverse . checkHints)

rethrowWithPositionTC
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => SourceSpan
  -> m a
  -> m a
rethrowWithPositionTC pos = withErrorMessageHint (positionedError pos)

warnAndRethrowWithPositionTC
  :: (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => SourceSpan
  -> m a
  -> m a
warnAndRethrowWithPositionTC pos = rethrowWithPositionTC pos . warnWithPosition pos

-- | Temporarily make a collection of type class dictionaries available
withTypeClassDictionaries
  :: MonadState CheckState m
  => [NamedDict]
  -> m a
  -> m a
withTypeClassDictionaries entries action = do
  orig <- get

  let mentries =
        M.fromListWith (M.unionWith (M.unionWith (<>)))
          [ (mn, M.singleton className (M.singleton (tcdValue entry) (pure entry)))
          | entry@TypeClassDictionaryInScope{ tcdValue = Qualified mn _, tcdClassName = className }
              <- entries
          ]

  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = M.unionWith (M.unionWith (M.unionWith (<>))) (typeClassDictionaries . checkEnv $ st) mentries } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = typeClassDictionaries . checkEnv $ orig } }
  return a

-- | Get the currently available map of type class dictionaries
getTypeClassDictionaries
  :: (MonadState CheckState m)
  => m (M.Map (Maybe ModuleName) (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict))))
getTypeClassDictionaries = typeClassDictionaries . checkEnv <$> get

-- | Lookup type class dictionaries in a module.
lookupTypeClassDictionaries
  :: (MonadState CheckState m)
  => Maybe ModuleName
  -> m (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict)))
lookupTypeClassDictionaries mn = fromMaybe M.empty . M.lookup mn . typeClassDictionaries . checkEnv <$> get

-- | Lookup type class dictionaries in a module.
lookupTypeClassDictionariesForClass
  :: (MonadState CheckState m)
  => Maybe ModuleName
  -> Qualified (ProperName 'ClassName)
  -> m (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict))
lookupTypeClassDictionariesForClass mn cn = fromMaybe M.empty . M.lookup cn <$> lookupTypeClassDictionaries mn

-- | Temporarily bind a collection of names to local variables
bindLocalVariables
  :: (MonadState CheckState m)
  => [(Ident, SourceType, NameVisibility)]
  -> m a
  -> m a
bindLocalVariables bindings =
  bindNames (M.fromList $ flip map bindings $ \(name, ty, visibility) -> (Qualified Nothing name, (ty, Private, visibility)))

-- | Temporarily bind a collection of names to local type variables
bindLocalTypeVariables
  :: (MonadState CheckState m)
  => ModuleName
  -> [(ProperName 'TypeName, SourceType)]
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
  -> m SourceType
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
  -> m SourceType
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

unsafeCheckCurrentModule
  :: forall m
   . (MonadError MultipleErrors m, MonadState CheckState m)
  => m ModuleName
unsafeCheckCurrentModule = checkCurrentModule <$> get >>= \case
  Nothing -> internalError "No module name set in scope"
  Just name -> pure name

debugEnv :: Environment -> [String]
debugEnv env = join
  [ debugTypes env
  , debugTypeSynonyms env
  , debugTypeClasses env
  , debugTypeClassDictionaries env
  , debugDataConstructors env
  , debugNames env
  ]

debugType :: Type a -> String
debugType = init . prettyPrintType 100

debugConstraint :: Constraint a -> String
debugConstraint (Constraint ann clsName kinds args _) =
  debugType $ foldl (TypeApp ann) (foldl (KindApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) kinds) args

debugTypes :: Environment -> [String]
debugTypes = go <=< M.toList . types
  where
  go (qual, (srcTy, which)) = do
    let
      ppTy = prettyPrintType 100 srcTy
      name = showQualified runProperName qual
      decl = case which of
        DataType _ _ _    -> "data"
        TypeSynonym       -> "type"
        ExternData _      -> "extern"
        LocalTypeVariable -> "local"
        ScopedTypeVar     -> "scoped"
    guard (not (isPrefixOf "Prim" name))
    pure $ decl <> " " <> unpack name <> " :: " <> init ppTy

debugNames :: Environment -> [String]
debugNames = fmap go . M.toList . names
  where
  go (qual, (srcTy, _, _)) = do
    let
      ppTy = prettyPrintType 100 srcTy
      name = showQualified runIdent qual
    unpack name <> " :: " <> init ppTy

debugDataConstructors :: Environment -> [String]
debugDataConstructors = fmap go . M.toList . dataConstructors
  where
  go (qual, (_, _, ty, _)) = do
    let
      ppTy = prettyPrintType 100 ty
      name = showQualified runProperName qual
    unpack name <> " :: " <> init ppTy

debugTypeSynonyms :: Environment -> [String]
debugTypeSynonyms = fmap go . M.toList . typeSynonyms
  where
  go (qual, (binders, subTy)) = do
    let
      vars = intercalate " " $ flip fmap binders $ \case
               (v, Just k) -> "(" <> unpack v <> " :: " <> init (prettyPrintType 100 k) <> ")"
               (v, Nothing) -> unpack v
      ppTy = prettyPrintType 100 subTy
      name = showQualified runProperName qual
    "type " <> unpack name <> " " <> vars <> " = " <> init ppTy

debugTypeClassDictionaries :: Environment -> [String]
debugTypeClassDictionaries = go . typeClassDictionaries
  where
  go tcds = do
    (mbModuleName, classes) <- M.toList tcds
    (className, instances) <- M.toList classes
    (ident, dicts) <- M.toList instances
    let
      moduleName = maybe "" (\m -> "[" <> runModuleName m <> "] ") mbModuleName
      className' = showQualified runProperName className
      ident' = showQualified runIdent ident
      kds = intercalate " " $ fmap ((\a -> "@(" <> a <> ")") . debugType) $ tcdInstanceKinds $ NEL.head dicts
      tys = intercalate " " $ fmap ((\a -> "(" <> a <> ")") . debugType) $ tcdInstanceTypes $ NEL.head dicts
    pure $ "dict " <> unpack moduleName <> unpack className' <> " " <> unpack ident' <> " (" <> show (length dicts) <> ")" <> " " <> kds <> " " <> tys

debugTypeClasses :: Environment -> [String]
debugTypeClasses = fmap go . M.toList . typeClasses
  where
  go (className, tc) = do
    let
      className' = showQualified runProperName className
      args = intercalate " " $ fmap (\(a, b) -> "(" <> debugType (maybe (srcTypeVar a) (srcKindedType (srcTypeVar a)) b) <> ")") $ typeClassArguments tc
    "class " <> unpack className' <> " " <> args

debugValue :: Expr -> String
debugValue = init . render . prettyPrintValue 100

debugSubstitution :: Substitution -> [String]
debugSubstitution (Substitution solved unsolved) =
  fmap go1 (M.toList solved) <> fmap go2 (M.toList unsolved')
  where
  unsolved' =
    M.filterWithKey (\k _ -> M.notMember k solved) unsolved

  go1 (u, ty) =
    "?" <> show u <> " = " <> debugType ty

  go2 (u, (_, k)) =
    "?" <> show u <> " :: " <> debugType k
