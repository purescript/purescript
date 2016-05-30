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
import Control.Monad.Writer.Class (MonadWriter(..))

import qualified Data.Map as M

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
  go Star Star = return ()
  go Bang Bang = return ()
  go Symbol Symbol = return ()
  go (Row k1') (Row k2') = go k1' k2'
  go (FunKind k1' k2') (FunKind k3 k4) = do
    go k1' k3
    go k2' k4
  go k1' k2' = throwError . errorMessage $ KindsDoNotUnify k1' k2'

-- | Infer the kind of a single type
kindOf
  :: (MonadError MultipleErrors m, MonadState CheckState m, MonadWriter MultipleErrors m)
  => Type
  -> m Kind
kindOf ty = fst <$> kindOfWithScopedVars ty

-- | Infer the kind of a single type, returning the kinds of any scoped type variables
kindOfWithScopedVars ::
  (MonadError MultipleErrors m, MonadState CheckState m, MonadWriter MultipleErrors m) =>
  Type ->
  m (Kind, [(String, Kind)])
kindOfWithScopedVars ty =
  rethrow (addHint (ErrorCheckingKind ty)) $
    fmap tidyUp . liftUnify $ infer ty
  where
  tidyUp ((k, args), sub) = ( starIfUnknown (substituteKind sub k)
                            , map (second (starIfUnknown . substituteKind sub)) args
                            )

-- | Infer the kind of a type constructor with a collection of arguments and a collection of associated data constructors
kindsOf
  :: (MonadError MultipleErrors m, MonadState CheckState m, MonadWriter MultipleErrors m)
  => Bool
  -> ModuleName
  -> ProperName 'TypeName
  -> [(String, Maybe Kind)]
  -> [Type]
  -> m Kind
kindsOf isData moduleName name args ts = fmap tidyUp . liftUnify $ do
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
  => (String, Maybe Kind)
  -> Kind
  -> m (ProperName 'TypeName, Kind)
freshKindVar (arg, Nothing) kind = return (ProperName arg, kind)
freshKindVar (arg, Just kind') kind = do
  unifyKinds kind kind'
  return (ProperName arg, kind')

-- | Simultaneously infer the kinds of several mutually recursive type constructors
kindsOfAll
  :: (MonadError MultipleErrors m, MonadState CheckState m, MonadWriter MultipleErrors m)
  => ModuleName
  -> [(ProperName 'TypeName, [(String, Maybe Kind)], Type)]
  -> [(ProperName 'TypeName, [(String, Maybe Kind)], [Type])]
  -> m ([Kind], [Kind])
kindsOfAll moduleName syns tys = fmap tidyUp . liftUnify $ do
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
    unifyKinds tyCon (foldr FunKind Star kargs)
    forM_ ks $ \k -> unifyKinds k Star
  unless isData $
    unifyKinds tyCon (foldr FunKind (head ks) kargs)
  return tyCon

-- | Default all unknown kinds to the Star kind of types
starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (Row k) = Row (starIfUnknown k)
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

-- | Infer a kind for a type
infer
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Type
  -> m (Kind, [(String, Kind)])
infer ty = rethrow (addHint (ErrorCheckingKind ty)) $ infer' ty

infer'
  :: forall m
   . (MonadError MultipleErrors m, MonadState CheckState m)
  => Type
  -> m (Kind, [(String, Kind)])
infer' (ForAll ident ty _) = do
  k1 <- freshKind
  Just moduleName <- checkCurrentModule <$> get
  (k2, args) <- bindLocalTypeVariables moduleName [(ProperName ident, k1)] $ infer ty
  unifyKinds k2 Star
  return (Star, (ident, k1) : args)
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
    unifyKinds k2 Star
    return Star
  go (KindedType ty k) = do
    k' <- go ty
    unifyKinds k k'
    return k'
  go TypeWildcard{} = freshKind
  go (TypeLevelString _) = return Symbol
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
  go (ConstrainedType deps ty) = do
    forM_ deps $ \(Constraint className tys _) -> do
      k <- go $ foldl TypeApp (TypeConstructor (fmap coerceProperName className)) tys
      unifyKinds k Star
    k <- go ty
    unifyKinds k Star
    return Star
  go ty = internalError $ "Invalid argument to infer: " ++ show ty
