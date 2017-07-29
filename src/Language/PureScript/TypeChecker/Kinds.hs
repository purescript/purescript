{-# LANGUAGE FlexibleInstances #-}

-- |
-- This module implements the kind checker
--
module Language.PureScript.TypeChecker.Kinds
  ( kindOf
  , kindOfWithScopedVars
  , kindsOf
  , kindsOfAll
  ) where

import Prelude.Compat

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State

import qualified Data.Map as M
import Data.Text (Text)

import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- | Generate a fresh kind variable
freshKind :: (MonadState CheckState m) => m Kind
freshKind = do
  k <- gets checkNextKind
  modify $ \st -> st { checkNextKind = k + 1 }
  return $ KUnknown k

-- | Update the substitution to solve a kind constraint
solveKind
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Int
  -> Kind
  -> m ()
solveKind u k = do
  occursCheck u k
  modify $ \cs -> cs { checkSubstitution =
                         (checkSubstitution cs) { substKind =
                                                    M.insert u k $ substKind $ checkSubstitution cs
                                                }
                     }

-- | Apply a substitution to a kind
substituteKind :: Substitution -> Kind -> Kind
substituteKind sub = everywhereOnKinds go
  where
  go (KUnknown u) =
    case M.lookup u (substKind sub) of
      Nothing -> KUnknown u
      Just (KUnknown u1) | u1 == u -> KUnknown u1
      Just t -> substituteKind sub t
  go other = other

-- | Make sure that an unknown does not occur in a kind
occursCheck
  :: (MonadError MultipleErrors m)
  => Int
  -> Kind
  -> m ()
occursCheck _ KUnknown{} = return ()
occursCheck u k = void $ everywhereOnKindsM go k
  where
  go (KUnknown u') | u == u' = throwError . errorMessage . InfiniteKind $ k
  go other = return other

-- | Unify two kinds
unifyKinds
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Kind
  -> Kind
  -> m ()
unifyKinds k1 k2 = do
  sub <- gets checkSubstitution
  go (substituteKind sub k1) (substituteKind sub k2)
  where
  go (KUnknown u1) (KUnknown u2) | u1 == u2 = return ()
  go (KUnknown u) k = solveKind u k
  go k (KUnknown u) = solveKind u k
  go (NamedKind k1') (NamedKind k2') | k1' == k2' = return ()
  go (Row k1') (Row k2') = unifyKinds k1' k2'
  go (FunKind k1' k2') (FunKind k3 k4) = do
    unifyKinds k1' k3
    unifyKinds k2' k4
  go k1' k2' = throwError . errorMessage $ KindsDoNotUnify k1' k2'

-- | Infer the kind of a single type
kindOf
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Type
  -> m Kind
kindOf ty = fst <$> kindOfWithScopedVars ty

-- | Infer the kind of a single type, returning the kinds of any scoped type variables
kindOfWithScopedVars ::
  (MonadError MultipleErrors m, MonadState CheckState m) =>
  Type ->
  m (Kind, [(Text, Kind)])
kindOfWithScopedVars ty =
  withErrorMessageHint (ErrorCheckingKind ty) $
    fmap tidyUp . withFreshSubstitution . captureSubstitution $ infer ty
  where
  tidyUp ((k, args), sub) = ( starIfUnknown (substituteKind sub k)
                            , map (second (starIfUnknown . substituteKind sub)) args
                            )

-- | Infer the kind of a type constructor with a collection of arguments and a collection of associated data constructors
kindsOf
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Bool
  -> ModuleName
  -> ProperName 'TypeName
  -> [(Text, Maybe Kind)]
  -> [Type]
  -> m Kind
kindsOf isData moduleName name args ts = fmap tidyUp . withFreshSubstitution . captureSubstitution $ do
  tyCon <- freshKind
  kargs <- replicateM (length args) freshKind
  rest <- zipWithM freshKindVar args kargs
  let dict = (name, tyCon) : rest
  bindLocalTypeVariables moduleName dict $
    solveTypes isData ts kargs tyCon
  where
  tidyUp (k, sub) = starIfUnknown $ substituteKind sub k

freshKindVar
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => (Text, Maybe Kind)
  -> Kind
  -> m (ProperName 'TypeName, Kind)
freshKindVar (arg, Nothing) kind = return (ProperName arg, kind)
freshKindVar (arg, Just kind') kind = do
  unifyKinds kind kind'
  return (ProperName arg, kind')

-- | Simultaneously infer the kinds of several mutually recursive type constructors
kindsOfAll
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> [(ProperName 'TypeName, [(Text, Maybe Kind)], Type)]
  -> [(ProperName 'TypeName, [(Text, Maybe Kind)], [Type])]
  -> m ([Kind], [Kind])
kindsOfAll moduleName syns tys = fmap tidyUp . withFreshSubstitution . captureSubstitution $ do
  synVars <- replicateM (length syns) freshKind
  let dict = zipWith (\(name, _, _) var -> (name, var)) syns synVars
  bindLocalTypeVariables moduleName dict $ do
    tyCons <- replicateM (length tys) freshKind
    let dict' = zipWith (\(name, _, _) tyCon -> (name, tyCon)) tys tyCons
    bindLocalTypeVariables moduleName dict' $ do
      data_ks <- zipWithM (\tyCon (_, args, ts) -> do
        kargs <- replicateM (length args) freshKind
        argDict <- zipWithM freshKindVar args kargs
        bindLocalTypeVariables moduleName argDict $
          solveTypes True ts kargs tyCon) tyCons tys
      syn_ks <- zipWithM (\synVar (_, args, ty) -> do
        kargs <- replicateM (length args) freshKind
        argDict <- zipWithM freshKindVar args kargs
        bindLocalTypeVariables moduleName argDict $
          solveTypes False [ty] kargs synVar) synVars syns
      return (syn_ks, data_ks)
  where
  tidyUp ((ks1, ks2), sub) = (map (starIfUnknown . substituteKind sub) ks1, map (starIfUnknown . substituteKind sub) ks2)

-- | Solve the set of kind constraints associated with the data constructors for a type constructor
solveTypes
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Bool
  -> [Type]
  -> [Kind]
  -> Kind
  -> m Kind
solveTypes isData ts kargs tyCon = do
  ks <- traverse (fmap fst . infer) ts
  when isData $ do
    unifyKinds tyCon (foldr FunKind kindType kargs)
    forM_ ks $ \k -> unifyKinds k kindType
  unless isData $
    unifyKinds tyCon (foldr FunKind (head ks) kargs)
  return tyCon

-- | Default all unknown kinds to the kindType kind of types
starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = kindType
starIfUnknown (Row k) = Row (starIfUnknown k)
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

-- | Infer a kind for a type
infer
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Type
  -> m (Kind, [(Text, Kind)])
infer ty = withErrorMessageHint (ErrorCheckingKind ty) $ infer' ty

infer'
  :: forall m
   . (MonadError MultipleErrors m, MonadState CheckState m)
  => Type
  -> m (Kind, [(Text, Kind)])
infer' (ForAll ident ty _) = do
  k1 <- freshKind
  Just moduleName <- checkCurrentModule <$> get
  (k2, args) <- bindLocalTypeVariables moduleName [(ProperName ident, k1)] $ infer ty
  unifyKinds k2 kindType
  return (kindType, (ident, k1) : args)
infer' (KindedType ty k) = do
  (k', args) <- infer ty
  unifyKinds k k'
  return (k', args)
infer' other = (, []) <$> go other
  where
  go :: Type -> m Kind
  go (ForAll ident ty _) = do
    k1 <- freshKind
    Just moduleName <- checkCurrentModule <$> get
    k2 <- bindLocalTypeVariables moduleName [(ProperName ident, k1)] $ go ty
    unifyKinds k2 kindType
    return kindType
  go (KindedType ty k) = do
    k' <- go ty
    unifyKinds k k'
    return k'
  go TypeWildcard{} = freshKind
  go TUnknown{} = freshKind
  go (TypeLevelString _) = return kindSymbol
  go (TypeVar v) = do
    Just moduleName <- checkCurrentModule <$> get
    lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
  go (Skolem v _ _ _) = do
    Just moduleName <- checkCurrentModule <$> get
    lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
  go (TypeConstructor v) = do
    env <- getEnv
    case M.lookup v (types env) of
      Nothing -> throwError . errorMessage . UnknownName $ fmap TyName v
      Just (kind, _) -> return kind
  go (TypeApp t1 t2) = do
    k0 <- freshKind
    k1 <- go t1
    k2 <- go t2
    unifyKinds k1 (FunKind k2 k0)
    return k0
  go REmpty = do
    k <- freshKind
    return $ Row k
  go (RCons _ ty row) = do
    k1 <- go ty
    k2 <- go row
    unifyKinds k2 (Row k1)
    return $ Row k1
  go (ConstrainedType (Constraint className tys _) ty) = do
    k1 <- go $ foldl TypeApp (TypeConstructor (fmap coerceProperName className)) tys
    unifyKinds k1 kindType
    k2 <- go ty
    unifyKinds k2 kindType
    return kindType
  go ty = internalError $ "Invalid argument to infer: " ++ show ty
