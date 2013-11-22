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
    StandaloneDeriving, MultiParamTypeClasses, FlexibleContexts #-}

module Language.PureScript.TypeChecker.Monad where

import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Unknown

import Data.Data
import Data.Maybe
import Data.Monoid
import Data.Typeable
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
                             }

newtype Check a = Check { unCheck :: StateT CheckState (Either String) a }
  deriving (Functor, Monad, Applicative, MonadPlus, MonadState CheckState, MonadError String)

newtype Substitution = Substitution { runSubstitution :: forall t. (Unifiable t) => Unknown t -> t }

instance Monoid Substitution where
  mempty = Substitution unknown
  s1 `mappend` s2 = Substitution $ \u -> apply s1 (apply s2 (unknown u))

newtype Subst m a = Subst { unSubst :: StateT Substitution m a }
  deriving (Functor, Monad, Applicative, MonadPlus, MonadTrans)

deriving instance (MonadError String m) => MonadError String (Subst m)

runSubst :: (Unifiable a, Monad m) => Subst m a -> m a
runSubst subst = do
  (a, s) <- flip runStateT mempty . unSubst $ subst
  return $ apply s a

substituteWith :: (Typeable t) => (Unknown t -> t) -> Substitution
substituteWith f = Substitution $ \u -> fromMaybe (unknown u) $ do
  u1 <- cast u
  cast (f u1)

substituteOne :: (Unifiable t) => Unknown t -> t -> Substitution
substituteOne u t = substituteWith $ \u1 ->
  case u1 of
    u2 | u2 == u -> t
       | otherwise -> unknown u2

replace :: (Unifiable t) => Unknown t -> t -> Subst Check ()
replace u t' = do
  sub <- Subst get
  let t = apply sub t'
  occursCheck u t
  let current = apply sub $ unknown u
  case isUnknown current of
    Just u1 | u1 == u -> return ()
    _ -> current ~~ t
  Subst . modify $ \s -> substituteOne u t <> s

class (Typeable t, Data t) => Unifiable t where
  unknown :: Unknown t -> t
  (~~) :: t -> t -> Subst Check ()
  isUnknown :: t -> Maybe (Unknown t)
  apply :: Substitution -> t -> t
  unknowns :: t -> [Int]

occursCheck :: (Unifiable t) => Unknown s -> t -> Subst Check ()
occursCheck (Unknown u) t =
  case isUnknown t of
    Nothing -> guardWith "Occurs check fails" (u `notElem` unknowns t)
    _ -> return ()

getEnv :: Check Environment
getEnv = fmap checkEnv get

putEnv :: Environment -> Check ()
putEnv env = modify (\s -> s { checkEnv = env })

modifyEnv :: (Environment -> Environment) -> Check ()
modifyEnv f = modify (\s -> s { checkEnv = f (checkEnv s) })

fresh' :: Subst Check Int
fresh' = do
  n <- lift $ checkNextVar <$> get
  lift . modify $ \s -> s { checkNextVar = succ (checkNextVar s) }
  return n

fresh :: (Unifiable t) => Subst Check t
fresh = unknown . Unknown <$> fresh'

runCheck :: Check a -> Either String (a, Environment)
runCheck c = do
  (a, s) <- flip runStateT (CheckState emptyEnvironment 0 global) $ unCheck c
  return (a, checkEnv s)

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
