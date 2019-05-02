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

import Data.Functor (($>))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Traversable (for)

import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- | Generate a fresh kind variable
freshKind :: (MonadState CheckState m) => SourceAnn -> m SourceKind
freshKind ann = do
  k <- gets checkNextKind
  modify $ \st -> st { checkNextKind = k + 1 }
  return $ KUnknown ann k

freshKind' :: (MonadState CheckState m) => m SourceKind
freshKind' = freshKind NullSourceAnn

-- | Update the substitution to solve a kind constraint
solveKind
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Int
  -> SourceKind
  -> m ()
solveKind u k = do
  occursCheck u k
  modify $ \cs -> cs { checkSubstitution =
                         (checkSubstitution cs) { substKind =
                                                    M.insert u k $ substKind $ checkSubstitution cs
                                                }
                     }

-- | Apply a substitution to a kind
substituteKind :: Substitution -> SourceKind -> SourceKind
substituteKind sub = everywhereOnKinds go
  where
  go (KUnknown ann u) =
    case M.lookup u (substKind sub) of
      Nothing -> KUnknown ann u
      Just (KUnknown ann' u1) | u1 == u -> KUnknown ann' u1
      Just t -> substituteKind sub t
  go other = other

-- | Make sure that an unknown does not occur in a kind
occursCheck
  :: (MonadError MultipleErrors m)
  => Int
  -> SourceKind
  -> m ()
occursCheck _ KUnknown{} = return ()
occursCheck u k = void $ everywhereOnKindsM go k
  where
  go (KUnknown _ u') | u == u' = throwError . errorMessage . InfiniteKind $ k
  go other = return other

-- | Unify two kinds
unifyKinds
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceKind
  -> SourceKind
  -> m ()
unifyKinds k1 k2 = do
  sub <- gets checkSubstitution
  go (substituteKind sub k1) (substituteKind sub k2)
  where
  go (KUnknown _ u1) (KUnknown _ u2) | u1 == u2 = return ()
  go (KUnknown _ u) k = solveKind u k
  go k (KUnknown _ u) = solveKind u k
  go (NamedKind _ k1') (NamedKind _ k2') | k1' == k2' = return ()
  go (Row _ k1') (Row _ k2') = unifyKinds k1' k2'
  go (FunKind _ k1' k2') (FunKind _ k3 k4) = do
    unifyKinds k1' k3
    unifyKinds k2' k4
  go k1' k2' =
    throwError
      . errorMessage''' (fst . getAnnForKind <$> [k1', k2'])
      $ KindsDoNotUnify k1' k2'

-- | Infer the kind of a single type
kindOf
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceType
  -> m SourceKind
kindOf ty = fst <$> kindOfWithScopedVars ty

-- | Infer the kind of a single type, returning the kinds of any scoped type variables
kindOfWithScopedVars ::
  (MonadError MultipleErrors m, MonadState CheckState m) =>
  SourceType ->
  m (SourceKind, [(Text, SourceKind)])
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
  -> [(Text, Maybe SourceKind)]
  -> [SourceType]
  -> m SourceKind
kindsOf isData moduleName name args ts = fmap tidyUp . withFreshSubstitution . captureSubstitution $ do
  tyCon <- freshKind'
  kargs <- replicateM (length args) $ freshKind'
  rest <- zipWithM freshKindVar args kargs
  let dict = (name, tyCon) : rest
  bindLocalTypeVariables moduleName dict $
    solveTypes isData ts kargs tyCon
  where
  tidyUp (k, sub) = starIfUnknown $ substituteKind sub k

freshKindVar
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => (Text, Maybe SourceKind)
  -> SourceKind
  -> m (ProperName 'TypeName, SourceKind)
freshKindVar (arg, Nothing) kind = return (ProperName arg, kind)
freshKindVar (arg, Just kind') kind = do
  unifyKinds kind kind'
  return (ProperName arg, kind')

-- | Simultaneously infer the kinds of several mutually recursive type constructors
kindsOfAll
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> [(SourceAnn, ProperName 'TypeName, [(Text, Maybe SourceKind)], SourceType)]
  -> [(SourceAnn, ProperName 'TypeName, [(Text, Maybe SourceKind)], [SourceType])]
  -> m ([SourceKind], [SourceKind])
kindsOfAll moduleName syns tys = fmap tidyUp . withFreshSubstitution . captureSubstitution $ do
  synVars <- for syns $ \(sa, _, _, _) -> freshKind sa
  let dict = zipWith (\(_, name, _, _) var -> (name, var)) syns synVars
  bindLocalTypeVariables moduleName dict $ do
    tyCons <- for tys $ \(sa, _, _, _) -> freshKind sa
    let dict' = zipWith (\(_, name, _, _) tyCon -> (name, tyCon)) tys tyCons
    bindLocalTypeVariables moduleName dict' $ do
      data_ks <- zipWithM (\tyCon (_, _, args, ts) -> do
        kargs <- for args $ \(_, kind) -> maybe freshKind' (freshKind . getAnnForKind) kind
        argDict <- zipWithM freshKindVar args kargs
        bindLocalTypeVariables moduleName argDict $
          solveTypes True ts kargs tyCon) tyCons tys
      syn_ks <- zipWithM (\synVar (_, _, args, ty) -> do
        kargs <- for args $ \(_, kind) -> maybe freshKind' (freshKind . getAnnForKind) kind
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
  -> [SourceType]
  -> [SourceKind]
  -> SourceKind
  -> m SourceKind
solveTypes isData ts kargs tyCon = do
  ks <- traverse (fmap fst . infer) ts
  when isData $ do
    unifyKinds tyCon (foldr srcFunKind kindType kargs)
    forM_ ks $ \k -> unifyKinds k (kindType $> getAnnForKind k)
  unless isData $
    unifyKinds tyCon (foldr srcFunKind (head ks) kargs)
  return tyCon

-- | Default all unknown kinds to the kindType kind of types
starIfUnknown :: Kind a -> Kind a
starIfUnknown (KUnknown ann _) = kindType $> ann
starIfUnknown (Row ann k) = Row ann (starIfUnknown k)
starIfUnknown (FunKind ann k1 k2) = FunKind ann (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

-- | Infer a kind for a type
infer
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceType
  -> m (SourceKind, [(Text, SourceKind)])
infer ty =
  withErrorMessageHint (ErrorCheckingKind ty)
    . rethrowWithPosition (fst $ getAnnForType ty)
    $ infer' ty

infer'
  :: forall m
   . (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceType
  -> m (SourceKind, [(Text, SourceKind)])
infer' (ForAll ann ident mbK ty _) = do
  k1 <- maybe (freshKind ann) pure mbK
  moduleName <- unsafeCheckCurrentModule
  (k2, args) <- bindLocalTypeVariables moduleName [(ProperName ident, k1)] $ infer ty
  unifyKinds k2 kindType
  return (kindType, (ident, k1) : args)
infer' (KindedType _ ty k) = do
  (k', args) <- infer ty
  unifyKinds k k'
  return (k', args)
infer' other = (, []) <$> go other
  where
  go :: SourceType -> m SourceKind
  go (ForAll ann ident mbK ty _) = do
    k1 <- maybe (freshKind ann) pure mbK
    moduleName <- unsafeCheckCurrentModule
    k2 <- bindLocalTypeVariables moduleName [(ProperName ident, k1)] $ go ty
    unifyKinds k2 kindType
    return $ kindType $> ann
  go (KindedType _ ty k) = do
    k' <- go ty
    unifyKinds k k'
    return k'
  go (TypeWildcard ann _) = freshKind ann
  go (TUnknown ann _) = freshKind ann
  go (TypeLevelString ann _) = return $ kindSymbol $> ann
  go (TypeVar ann v) = do
    moduleName <- unsafeCheckCurrentModule
    ($> ann) <$> lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
  go (Skolem ann v _ _) = do
    moduleName <- unsafeCheckCurrentModule
    ($> ann) <$> lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
  go (TypeConstructor ann v) = do
    env <- getEnv
    case M.lookup v (types env) of
      Nothing -> throwError . errorMessage' (fst ann) . UnknownName $ fmap TyName v
      Just (kind, _) -> return $ kind $> ann
  go (TypeApp ann t1 t2) = do
    k0 <- freshKind ann
    k1 <- go t1
    k2 <- go t2
    unifyKinds k1 (FunKind ann k2 k0)
    return k0
  go (REmpty ann) = do
    k <- freshKind ann
    return $ Row ann k
  go (RCons ann _ ty row) = do
    k1 <- go ty
    k2 <- go row
    unifyKinds k2 (Row ann k1)
    return $ Row ann k1
  go (ConstrainedType ann2 (Constraint ann1 className tys _) ty) = do
    k1 <- go $ foldl (TypeApp ann2) (TypeConstructor ann1 (fmap coerceProperName className)) tys
    unifyKinds k1 kindType
    k2 <- go ty
    unifyKinds k2 kindType
    return $ kindType $> ann2
  go ty = internalError $ "Invalid argument to infer: " ++ show ty
