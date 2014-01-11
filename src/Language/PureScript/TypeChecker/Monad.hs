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

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, RankNTypes, DeriveDataTypeable,
    GADTs, StandaloneDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module Language.PureScript.TypeChecker.Monad where

import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Unknown

import Data.Data
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.Map as M

data NameKind
  = Value
  | Extern
  | Alias ModuleName Ident
  | LocalVariable
  | DataConstructor deriving Show

data TypeDeclarationKind
  = Data
  | ExternData
  | TypeSynonym
  | DataAlias ModuleName ProperName
  | LocalTypeVariable deriving Show

data Environment = Environment
  { names :: M.Map (ModuleName, Ident) (Type, NameKind)
  , types :: M.Map (ModuleName, ProperName) (Kind, TypeDeclarationKind)
  , dataConstructors :: M.Map (ModuleName, ProperName) (Type, NameKind)
  , typeSynonyms :: M.Map (ModuleName, ProperName) ([String], Type)
  , members :: M.Map (ModuleName, Ident) String
  } deriving (Show)

emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty M.empty M.empty M.empty M.empty

bindNames :: (MonadState CheckState m) => M.Map (ModuleName, Ident) (Type, NameKind) -> m a -> m a
bindNames newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { names = newNames `M.union` (names . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { names = names . checkEnv $ orig } }
  return a

bindTypes :: (MonadState CheckState m) => M.Map (ModuleName, ProperName) (Kind, TypeDeclarationKind) -> m a -> m a
bindTypes newNames action = do
  orig <- get
  modify $ \st -> st { checkEnv = (checkEnv st) { types = newNames `M.union` (types . checkEnv $ st) } }
  a <- action
  modify $ \st -> st { checkEnv = (checkEnv st) { types = types . checkEnv $ orig } }
  return a

bindLocalVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(Ident, Type)] -> m a -> m a
bindLocalVariables moduleName bindings action =
  bindNames (M.fromList $ flip map bindings $ \(name, ty) -> ((moduleName, name), (ty, LocalVariable))) action

bindLocalTypeVariables :: (Functor m, MonadState CheckState m) => ModuleName -> [(ProperName, Kind)] -> m a -> m a
bindLocalTypeVariables moduleName bindings action =
  bindTypes (M.fromList $ flip map bindings $ \(name, k) -> ((moduleName, name), (k, LocalTypeVariable))) action

lookupVariable :: (Functor m, MonadState CheckState m, MonadError String m) => ModuleName -> Qualified Ident -> m Type
lookupVariable currentModule (Qualified moduleName var) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, var) (names env) of
    Nothing -> throwError $ show var ++ " is undefined"
    Just (ty, _) -> return ty

lookupTypeVariable :: (Functor m, MonadState CheckState m, MonadError String m) => ModuleName -> Qualified ProperName -> m Kind
lookupTypeVariable currentModule (Qualified moduleName name) = do
  env <- getEnv
  case M.lookup (fromMaybe currentModule moduleName, name) (types env) of
    Nothing -> throwError $ "Type variable " ++ show name ++ " is undefined"
    Just (k, _) -> return k

canonicalize :: ModuleName -> Environment -> Qualified Ident -> (ModuleName, Ident)
canonicalize _ _ (Qualified (Just mn) i) = (mn, i)
canonicalize mn env (Qualified Nothing i) = case (mn, i) `M.lookup` names env of
  Just (_, Alias mn' i') -> (mn', i')
  _ -> (mn, i)

canonicalizeType :: ModuleName -> Environment -> Qualified ProperName -> (ModuleName, ProperName)
canonicalizeType _ _ (Qualified (Just mn) nm) = (mn, nm)
canonicalizeType mn env (Qualified Nothing nm) = case (mn, nm) `M.lookup` types env of
  Just (_, DataAlias mn' pn') -> (mn', pn')
  _ -> (mn, nm)

data AnyUnifiable where
  AnyUnifiable :: forall t. (Unifiable t) => t -> AnyUnifiable

data CheckState = CheckState { checkEnv :: Environment
                             , checkNextVar :: Int
                             }

newtype Check a = Check { unCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Monad, Applicative, MonadPlus, MonadState CheckState, MonadError String)

getEnv :: (Functor m, MonadState CheckState m) => m Environment
getEnv = checkEnv <$> get

putEnv :: (MonadState CheckState m) => Environment -> m ()
putEnv env = modify (\s -> s { checkEnv = env })

modifyEnv :: (MonadState CheckState m) => (Environment -> Environment) -> m ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

runCheck :: Check a -> Either String (a, Environment)
runCheck c = do
  (a, s) <- flip runStateT (CheckState emptyEnvironment 0) $ unCheck c
  return (a, checkEnv s)

guardWith :: (MonadError e m) => e -> Bool -> m ()
guardWith _ True = return ()
guardWith e False = throwError e

rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError $ \e -> throwError (f e)

newtype Substitution = Substitution { runSubstitution :: forall t. (Unifiable t) => Unknown t -> t }

instance Monoid Substitution where
  mempty = Substitution unknown
  s1 `mappend` s2 = Substitution $ \u -> apply s1 (apply s2 (unknown u))

data SubstState = SubstState { substSubst :: Substitution
                             , substFutureEscapeChecks :: [AnyUnifiable] }

newtype SubstContext = SubstContext { substCurrentModule :: ModuleName } deriving (Show)

newtype Subst a = Subst { unSubst :: ReaderT SubstContext (StateT SubstState Check) a }
  deriving (Functor, Monad, Applicative, MonadPlus, MonadReader SubstContext)

instance MonadState CheckState Subst where
  get = Subst . lift . lift $ get
  put = Subst . lift . lift . put

deriving instance MonadError String Subst

liftCheck :: Check a -> Subst a
liftCheck = Subst . lift . lift

runSubst :: (Unifiable a) => SubstContext -> Subst a -> Check (a, Substitution, [AnyUnifiable])
runSubst context subst = do
  (a, s) <- flip runStateT (SubstState mempty []) . flip runReaderT context . unSubst $ subst
  return (apply (substSubst s) a, substSubst s, substFutureEscapeChecks s)

substituteWith :: (Typeable t) => (Unknown t -> t) -> Substitution
substituteWith f = Substitution $ \u -> fromMaybe (unknown u) $ do
  u1 <- cast u
  cast (f u1)

substituteOne :: (Unifiable t) => Unknown t -> t -> Substitution
substituteOne u t = substituteWith $ \u1 ->
  case u1 of
    u2 | u2 == u -> t
       | otherwise -> unknown u2

replace :: (Unifiable t) => Unknown t -> t -> Subst ()
replace u t' = do
  sub <- substSubst <$> Subst get
  let t = apply sub t'
  occursCheck u t
  let current = apply sub $ unknown u
  case isUnknown current of
    Just u1 | u1 == u -> return ()
    _ -> current ~~ t
  Subst . modify $ \s -> s { substSubst = substituteOne u t <> substSubst s }

class (Typeable t, Data t, Show t) => Unifiable t where
  unknown :: Unknown t -> t
  (~~) :: t -> t -> Subst ()
  isUnknown :: t -> Maybe (Unknown t)
  apply :: Substitution -> t -> t
  unknowns :: t -> [Int]

instance (Unifiable a) => Unifiable [a] where
  unknown _ = error "not supported"
  (~~) = zipWithM_ (~~)
  isUnknown _ = error "not supported"
  apply s = map (apply s)
  unknowns = concatMap unknowns

occursCheck :: (Unifiable t) => Unknown s -> t -> Subst ()
occursCheck (Unknown u) t =
  case isUnknown t of
    Nothing -> guardWith "Occurs check fails" (u `notElem` unknowns t)
    _ -> return ()

fresh' :: Subst Int
fresh' = do
  n <- checkNextVar <$> get
  modify $ \s -> s { checkNextVar = succ (checkNextVar s) }
  return n

fresh :: (Unifiable t) => Subst t
fresh = unknown . Unknown <$> fresh'

escapeCheckLater :: (Unifiable t) => t -> Subst ()
escapeCheckLater t = Subst . modify $ \s -> s { substFutureEscapeChecks = AnyUnifiable t : substFutureEscapeChecks s  }
