{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Interaction solver for Coercible constraints
--
module Language.PureScript.TypeChecker.Entailment.Coercible
  ( GivenSolverState(..)
  , initialGivenSolverState
  , solveGivens
  , WantedSolverState(..)
  , initialWantedSolverState
  , solveWanteds
  , insoluble
  ) where

import Prelude hiding (interact)

import Control.Applicative ((<|>), empty)
import Control.Arrow ((&&&))
import Control.Monad ((<=<), guard, unless, when)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (StateT, get, gets, modify, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Writer (Writer, execWriter, runWriter, runWriterT, tell, WriterT)
import Data.Either (partitionEithers)
import Data.Foldable (fold, foldl', for_, toList)
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Any(..))
import Data.Text (Text)

import Data.Map qualified as M
import Data.Set qualified as S

import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), Environment(..), TypeKind(..), unapplyKinds)
import Language.PureScript.Errors (DeclarationRef(..), ErrorMessageHint(..), ExportSource, ImportDeclarationType(..), MultipleErrors, SimpleErrorMessage(..), SourceAnn, errorMessage, UnknownsHint(..))
import Language.PureScript.Names (ModuleName, ProperName, ProperNameType(..), Qualified(..), byMaybeModuleName, toMaybeModuleName)
import Language.PureScript.TypeChecker.Kinds (elaborateKind, freshKindWithKind, unifyKinds')
import Language.PureScript.TypeChecker.Monad (CheckState(..), TypeCheckM)
import Language.PureScript.TypeChecker.Roles (lookupRoles)
import Language.PureScript.TypeChecker.Synonyms (replaceAllTypeSynonyms)
import Language.PureScript.TypeChecker.Unify (alignRowsWith, freshTypeWithKind, substituteType)
import Language.PureScript.Roles (Role(..))
import Language.PureScript.Types (Constraint(..), SourceType, Type(..), completeBinderList, containsUnknowns, everythingOnTypes, isMonoType, replaceAllTypeVars, rowFromList, srcConstraint, srcTypeApp, unapplyTypes)
import Language.PureScript.Constants.Prim qualified as Prim

-- | State of the given constraints solver.
data GivenSolverState =
  GivenSolverState
    { inertGivens :: [(SourceType, SourceType, SourceType)]
  -- ^ A set of irreducible given constraints which do not interact together.
    , unsolvedGivens :: [(SourceType, SourceType)]
  -- ^ Given constraints yet to be solved.
    }

-- | Initialize the given constraints solver state with the givens to solve.
initialGivenSolverState :: [(SourceType, SourceType)] -> GivenSolverState
initialGivenSolverState =
  GivenSolverState []

-- | The given constraints solver follows these steps:
--
-- 1. Solving can diverge for recursive newtypes, so we check the solver depth
-- and abort if we crossed an arbitrary limit.
--
-- For instance the declarations:
--
-- @
-- newtype N a = N (a -> N a)
--
-- example :: forall a b. N a -> N b
-- example = coerce
-- @
--
-- yield the wanted @Coercible (N a) (N b)@ which we can unwrap on both sides
-- to yield @Coercible (a -> N a) (b -> N b)@, which we can then decompose back
-- to @Coercible a b@ and @Coercible (N a) (N b)@.
--
-- 2. We pick a constraint from the unsolved queue. If the queue is empty we are
-- done, otherwise we unify the constraint arguments kinds and continue.
--
-- 3. Then we try to canonicalize the constraint.

-- 3a. Canonicalization can fail, in which case we swallow the error and pretend
-- the constraint is irreducible because it is possible to eventually solve it.
--
-- For instance the declarations:
--
-- @
-- data D a = D a
-- type role D nominal
--
-- example :: forall a b. Coercible (D a) (D b) => D a -> D b
-- example = coerce
-- @
--
-- yield an insoluble given @Coercible (D a) (D b)@ which discharges the wanted
-- constraint regardless, because the given can be solved if @a@ and @b@ turn
-- out to be equal: @example (D true) :: D Boolean@ should compile.
--
-- 3b. Canonicalization can succeed with an irreducible constraint which we
-- then interact with the inert set.
--
-- 3bi. These interactions can yield a derived constraint which we add to the
-- unsolved queue and then go back to 1.
--
-- 3bii. These interactions can discharge the constraint, in which case we go
-- back to 1.
--
-- 3biii The constraint may not react to the inert set, in which case we add it
-- to the inert set, kick out any constraint that can be rewritten by the new
-- inert, add them to the unsolved queue and then go back to 1.
--
-- 3c. Otherwise canonicalization can succeed with derived constraints which we
-- add to the unsolved queue and then go back to 1.
solveGivens
  :: Environment
  -> StateT GivenSolverState TypeCheckM ()
solveGivens env = go (0 :: Int) where
  go n = do
    when (n > 1000) . throwError . errorMessage $ PossiblyInfiniteCoercibleInstance
    gets unsolvedGivens >>= \case
      [] -> pure ()
      given : unsolved -> do
        (k, a, b) <- lift $ unify given
        GivenSolverState{..} <- get
        lift (fst <$> runWriterT (canon env Nothing k a b `catchError` recover)) >>= \case
          Irreducible -> case interact env (a, b) inertGivens of
            Just (Simplified (a', b')) ->
              put $ GivenSolverState { unsolvedGivens = (a', b') : unsolved, .. }
            Just Discharged ->
              put $ GivenSolverState { unsolvedGivens = unsolved, .. }
            Nothing -> do
              let (kickedOut, kept) = partitionEithers $ kicksOut env (a, b) <$> inertGivens
              put $ GivenSolverState
                { inertGivens = (k, a, b) : kept
                , unsolvedGivens = kickedOut <> unsolved
                }
          Canonicalized deriveds ->
            put $ GivenSolverState { unsolvedGivens = toList deriveds <> unsolved, .. }
        go (n + 1)
  recover _ = pure Irreducible

-- | State of the wanted constraints solver.
data WantedSolverState =
  WantedSolverState
    { inertGivens :: [(SourceType, SourceType, SourceType)]
  -- ^ A set of irreducible given constraints which do not interact together,
  -- but which could interact with the wanteds.
    , inertWanteds :: [(SourceType, SourceType, SourceType)]
  -- ^ A set of irreducible wanted constraints which do not interact together,
  -- nor with any given.
    , unsolvedWanteds :: [(SourceType, SourceType)]
  -- ^ Wanted constraints yet to be solved.
    }

-- | Initialize the wanted constraints solver state with an inert set of givens
-- and the two parameters of the wanted to solve.
initialWantedSolverState
  :: [(SourceType, SourceType, SourceType)]
  -> SourceType
  -> SourceType
  -> WantedSolverState
initialWantedSolverState givens a b =
  WantedSolverState givens [] [(a, b)]

-- | The wanted constraints solver follows similar steps than the given solver,
-- except for:
--
-- 1. When canonicalization fails we can swallow the error, but only if the
-- wanted interacts with the givens.
--
-- For instance the declarations:
--
-- @
-- data D a = D a
-- type role D nominal
--
-- example :: forall a b. Coercible (D a) (D b) => D a -> D b
-- example = coerce
-- @
--
-- yield an insoluble wanted @Coercible (D a) (D b)@ which is discharged by
-- the given. But we want @example :: forall a b. D a -> D b@ to fail.
--
-- 2. Irreducible wanted constraints don't interact with the inert wanteds set,
-- because doing so would yield confusing error messages.
--
-- For instance the declarations:
--
-- @
-- data D a = D a
--
-- example :: forall a. D a a -> D Boolean Char
-- example = coerce
-- @
--
-- yield the wanted @Coercible (D a a) (D Boolean Char)@, which is decomposed to
-- the irreducibles @Coercible a Boolean@ and @Coercible a Char@. Would we
-- interact the latter with the former, we would report an insoluble
-- @Coercible Boolean Char@.
solveWanteds
  :: Environment
  -> StateT WantedSolverState CanonM ()
solveWanteds env = go (0 :: Int) where
  go n = do
    when (n > 1000) . throwError . errorMessage $ PossiblyInfiniteCoercibleInstance
    gets unsolvedWanteds >>= \case
      [] -> pure ()
      wanted : unsolved -> do
        (k, a, b) <- lift $ lift $ unify wanted
        WantedSolverState{..} <- get
        lift (canon env (Just inertGivens) k a b `catchError` recover (a, b) inertGivens) >>= \case
          Irreducible -> case interact env (a, b) inertGivens of
            Just (Simplified (a', b')) ->
              put $ WantedSolverState { unsolvedWanteds = (a', b') : unsolved, .. }
            Just Discharged ->
              put $ WantedSolverState { unsolvedWanteds = unsolved, .. }
            Nothing ->
              put $ WantedSolverState
                { inertWanteds = (k, a, b) : inertWanteds
                , unsolvedWanteds = unsolved
                , ..
                }
          Canonicalized deriveds ->
            put $ WantedSolverState { unsolvedWanteds = toList deriveds <> unsolved, .. }
        go (n + 1)
  recover wanted givens errors =
    case interact env wanted givens of
      Nothing -> throwError errors
      Just (Simplified wanted') -> pure . Canonicalized $ S.singleton wanted'
      Just Discharged -> pure $ Canonicalized mempty

-- | Unifying constraints arguments kinds isn't strictly necessary but yields
-- better error messages. For instance we cannot solve the constraint
-- @Coercible (D :: Type -> Type) (D a :: Type)@ because its arguments kinds
-- don't match and trying to unify them will say so, which is more helpful than
-- simply saying that no type class instance was found.
--
-- A subtle thing to note is that types with polymorphic kinds can be annotated
-- with kind applications mentioning unknowns that we may have solved by
-- unifying the kinds.
--
-- For instance the declarations:
--
-- @
-- data D :: forall k. k -> Type
-- data D a = D
--
-- type role D representational
--
-- example :: D D -> D D
-- example = coerce
-- @
--
-- yield a wanted
-- @Coercible (D \@(k1 -> Type) (D \@k1)) (D \@(k2 -> Type) (D \@k2))@, which we
-- decompose to @Coercible (D \@k1) (D \@k2)@, where @k1@ and @k2@ are unknowns.
-- This constraint is not reflexive because @D \@k1@ and @D \@k2@ are differents
-- but both arguments kinds unify with @k -> Type@, where @k@ is a fresh unknown,
-- so applying the substitution to @D \@k1@ and @D \@k2@ yields a
-- @Coercible (D \@k) (D \@k)@ constraint which could be trivially solved by
-- reflexivity instead of having to saturate the type constructors.
unify
  :: (SourceType, SourceType)
  -> TypeCheckM (SourceType, SourceType, SourceType)
unify (a, b) = do
  let kindOf = sequence . (id &&& elaborateKind) <=< replaceAllTypeSynonyms
  (a', kind) <- kindOf a
  (b', kind') <- kindOf b
  unifyKinds' kind kind'
  subst <- gets checkSubstitution
  pure ( substituteType subst kind
       , substituteType subst a'
       , substituteType subst b'
       )

-- | A successful interaction between an irreducible constraint and an inert
--  given constraint has two possible outcomes:
data Interaction
  = Simplified (SourceType, SourceType)
  -- ^ The interaction can yield a derived constraint,
  | Discharged
  -- ^ or we can learn the irreducible constraint is redundant and discharge it.

-- | Interact an irreducible constraint with an inert set of givens.
interact
  :: Environment
  -> (SourceType, SourceType)
  -> [(SourceType, SourceType, SourceType)]
  -> Maybe Interaction
interact env irred = go where
  go [] = Nothing
  go (inert : _)
    | canDischarge inert irred = Just Discharged
    | Just derived <- interactSameTyVar inert irred = Just $ Simplified derived
    | Just derived <- interactDiffTyVar env inert irred = Just $ Simplified derived
  go (_ : inerts) = go inerts

-- | A given constraint of the form @Coercible a b@ can discharge constraints
-- of the form @Coercible a b@ and @Coercible b a@.
canDischarge
  :: (SourceType, SourceType, SourceType)
  -> (SourceType, SourceType)
  -> Bool
canDischarge (_, a, b) constraint =
  (a, b) == constraint || (b, a) == constraint

-- | Two canonical constraints of the form @Coercible tv ty1@ and
-- @Coercible tv ty2@ can interact together and yield a new constraint
-- @Coercible ty1 ty2@. Canonicality matters to avoid loops.
--
-- For instance the declarations:
--
-- @
-- data D a = D a
-- newtype N a = N (D (N a))
--
-- example :: forall a. Coercible a (D a) => a -> N a
-- example = coerce
-- @
--
-- yield a non canonical wanted @Coercible a (N a)@ that we can unwrap on the
-- right to yield @Coercible a (D (N a))@. Would it interact with the non
-- canonical given @Coercible a (D a)@ it would give @Coercible (D a) (D (N a))@,
-- then decompose back to @Coercible a (N a)@.
interactSameTyVar
  :: (SourceType, SourceType, SourceType)
  -> (SourceType, SourceType)
  -> Maybe (SourceType, SourceType)
interactSameTyVar (_, tv1, ty1) (tv2, ty2)
  | tv1 == tv2 && isCanonicalTyVarEq (tv1, ty1) && isCanonicalTyVarEq (tv2, ty2)
  = Just (ty1, ty2)
  | otherwise = Nothing

-- | Two canonical constraints of the form @Coercible tv1 ty1@ and
-- @Coercible tv2 ty2@ can interact together and yield a new constraint
-- @Coercible tv2 ty2[ty1/tv1]@. Once again, canonicality matters to avoid loops.
--
-- For instance the declarations:
--
-- @
-- data D a = D a
--
-- example :: forall a b. Coercible b (D b) => a -> b
-- example = coerce
-- @
--
-- yield an irreducible canonical wanted @Coercible a b@. Would it interact with
-- the non canonical given @Coercible b (D b)@ it would give @Coercible a (D b)@,
-- which would keep interacting indefinitely with the given.
interactDiffTyVar
  :: Environment
  -> (SourceType, SourceType, SourceType)
  -> (SourceType, SourceType)
  -> Maybe (SourceType, SourceType)
interactDiffTyVar env (_, tv1, ty1) (tv2, ty2)
  | tv1 /= tv2 && isCanonicalTyVarEq (tv2, ty2)
  , (ty2', Any True) <- runWriter $ rewrite env (tv1, ty1) ty2
  = Just (tv2, ty2')
  | otherwise = Nothing

-- | A canonical constraint of the form @Coercible tv1 ty1@ can rewrite the
-- right hand side of an irreducible constraint of the form @Coercible tv2 ty2@
-- by substituting @ty1@ for every occurrence of @tv1@ at representational and
-- phantom role in @ty2@. Nominal occurrences are left untouched.
rewrite :: Environment -> (SourceType, SourceType) -> SourceType -> Writer Any SourceType
rewrite env (Skolem _ _ _ s1 _, ty1) | not $ occurs s1 ty1 = go where
  go (Skolem _ _ _ s2 _) | s1 == s2 = tell (Any True) $> ty1
  go ty2 | (Skolem{}, _, xs) <- unapplyTypes ty2, not $ null xs =
    rewriteTyVarApp go ty2
         | (TypeConstructor _ tyName, _, _) <- unapplyTypes ty2 = do
    rewriteTyConApp go (lookupRoles env tyName) ty2
  go (KindApp sa ty k) = KindApp sa <$> go ty <*> pure k
  go (ForAll sa vis tv k ty scope) = ForAll sa vis tv k <$> go ty <*> pure scope
  go (ConstrainedType sa Constraint{..} ty) | s1 `S.notMember` foldMap skolems constraintArgs =
    ConstrainedType sa Constraint{..} <$> go ty
  go (RCons sa label ty rest) = RCons sa label <$> go ty <*> go rest
  go (KindedType sa ty k) = KindedType sa <$> go ty <*> pure k
  go ty2 = pure ty2
rewrite _ _ = pure

-- | Rewrite the head of a type application of the form @tv a_0 .. a_n@.
rewriteTyVarApp
  :: Applicative m
  => (SourceType -> m SourceType)
  -> SourceType
  -> m SourceType
rewriteTyVarApp f = go where
  go (TypeApp sa lhs rhs) =
    TypeApp sa <$> go lhs <*> pure rhs
  go (KindApp sa ty k) =
    KindApp sa <$> go ty <*> pure k
  go ty = f ty

-- | Rewrite the representational and phantom arguments of a type application
-- of the form @D a_0 .. a_n@.
rewriteTyConApp
  :: Applicative m
  => (SourceType -> m SourceType)
  -> [Role]
  -> SourceType
  -> m SourceType
rewriteTyConApp f = go where
  go (role : roles) (TypeApp sa lhs rhs) =
    TypeApp sa <$> go roles lhs <*> case role of
      Nominal -> pure rhs
      _ -> f rhs
  go roles (KindApp sa ty k) =
    KindApp sa <$> go roles ty <*> pure k
  go _ ty = pure ty

canRewrite :: Environment -> (SourceType, SourceType) -> SourceType -> Bool
canRewrite env irred = getAny . execWriter . rewrite env irred

-- | An irreducible given constraint must kick out of the inert set any
-- constraint it can rewrite when it becomes inert, otherwise solving would be
-- sensitive to the order of constraints. Wanteds cannot rewrite other wanteds
-- so this applies only to givens.
--
-- For instance the declaration:
--
-- @
-- example :: forall f g a b. Coercible a (f b) => Coercible f g => Proxy f -> a -> g b
-- example _ = coerce
-- @
--
-- yields the irreducible givens @Coercible a (f b)@ and @Coercible f g@. Would
-- we not kick out the former when adding the latter to the inert set we would
-- not be able to rewrite it to @Coercible a (g b)@ and discharge the wanted,
-- but inverting the givens would work.
kicksOut
  :: Environment
  -> (SourceType, SourceType)
  -> (SourceType, SourceType, SourceType)
  -> Either (SourceType, SourceType) (SourceType, SourceType, SourceType)
kicksOut env irred (_, tv2, ty2)
  | isCanonicalTyVarEq (tv2, ty2) && canRewrite env irred ty2
  = Left (tv2, ty2)
kicksOut _ _ inert = Right inert

-- | A constraint of the form @Coercible tv ty@ is canonical when @tv@ does not
-- occur in @ty@. Non canonical constraints do not interact to prevent loops.
isCanonicalTyVarEq :: (SourceType, SourceType) -> Bool
isCanonicalTyVarEq (Skolem _ _ _ s _, ty) = not $ occurs s ty
isCanonicalTyVarEq _ = False

occurs :: Int -> SourceType -> Bool
occurs s1 = everythingOnTypes (||) go where
  go (Skolem _ _ _ s2 _) | s1 == s2 = True
  go _ = False

skolems :: SourceType -> S.Set Int
skolems = everythingOnTypes (<>) go where
  go (Skolem _ _ _ s _) = S.singleton s
  go _ = mempty

-- | A successful canonicalization result has two possible outcomes:
data Canonicalized
  = Canonicalized (S.Set (SourceType, SourceType))
  -- ^ Canonicalization can yield a set of derived constraints,
  | Irreducible
  -- ^ or we can learn the constraint is irreducible. Irreducibility is not
  -- necessarily an error, we may make further progress by interacting with
  -- inerts.

type CanonM = WriterT [ErrorMessageHint] TypeCheckM

-- | Canonicalization takes a wanted constraint and try to reduce it to a set of
-- simpler constraints whose satisfaction will imply the goal.
canon
  :: Environment
  -> Maybe [(SourceType, SourceType, SourceType)]
  -> SourceType
  -> SourceType
  -> SourceType
  -> CanonM Canonicalized
canon env givens k a b =
  maybe (throwError $ insoluble k a b) pure <=< runMaybeT $
        canonRefl a b
    <|> canonUnsaturatedHigherKindedType env a b
    <|> canonRow a b
    -- We unwrap newtypes before trying the decomposition rules because it let
    -- us solve more constraints.
    --
    -- For instance the declarations:
    --
    -- @
    -- newtype N f a = N (f a)
    --
    -- example :: forall a b. Coercible a b => N Maybe a -> N Maybe b
    -- example = coerce
    -- @
    --
    -- yield the wanted @Coercible (N Maybe a) (N Maybe b)@ which we cannot
    -- decompose because the second parameter of @N@ is nominal. On the other
    -- hand, unwrapping on both sides yields @Coercible (Maybe a) (Maybe b)@
    -- which we can then decompose to @Coercible a b@ and discharge with the
    -- given.
    <|> canonNewtypeLeft env a b
    <|> canonNewtypeRight env a b
    <|> canonDecomposition env a b
    <|> canonDecompositionFailure env k a b
    <|> canonNewtypeDecomposition env givens a b
    <|> canonNewtypeDecompositionFailure a b
    <|> canonTypeVars a b
    <|> canonTypeVarLeft a b
    <|> canonTypeVarRight a b
    <|> canonApplicationLeft a b
    <|> canonApplicationRight a b

insoluble
  :: SourceType
  -> SourceType
  -> SourceType
  -> MultipleErrors
insoluble k a b =
  -- We can erase kind applications when determining whether to show the
  -- "Consider adding a type annotation" hint, because annotating kinds to
  -- instantiate unknowns in Coercible constraints should never resolve
  -- NoInstanceFound errors.
  errorMessage $ NoInstanceFound (srcConstraint Prim.Coercible [k] [a, b] Nothing) [] 
    $ if any containsUnknowns [a, b] then Unknowns else NoUnknowns

-- | Constraints of the form @Coercible a b@ can be solved if the two arguments
-- are the same. Since we currently don't support higher-rank arguments in
-- instance heads, term equality is a sufficient notion of "the same".
canonRefl
  :: SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonRefl a b =
  guard (a == b) $> Canonicalized mempty

-- | Constraints of the form @Coercible (T1 a_0 .. a_n) (T2 b_0 .. b_n)@, where
-- both arguments have kind @k1 -> k2@, yield a constraint
-- @Coercible (T1 a_0 .. a_n c_0 .. c_m) (T2 b_0 .. b_n c_0 .. c_m)@, where both
-- arguments are fully saturated with the same unknowns and have kind @Type@.
canonUnsaturatedHigherKindedType
  :: Environment
  -> SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonUnsaturatedHigherKindedType env a b
  | (TypeConstructor _ aTyName, akapps, axs) <- unapplyTypes a
  , (ak, _) <- fromMaybe (internalError "canonUnsaturatedHigherKindedType: type lookup failed") $ M.lookup aTyName (types env)
  , (aks, _) <- unapplyKinds ak
  , length axs < length aks = do
      ak' <- lift $ do
        let (kvs, ak') = fromMaybe (internalError "canonUnsaturatedHigherKindedType: unkinded forall binder") $ completeBinderList ak
            instantiatedKinds = zipWith (\(_, (kv, _)) k -> (kv, k)) kvs akapps
        unknownKinds <- traverse (\((ss, _), (kv, k)) -> (kv,) <$> lift (freshKindWithKind ss k)) $ drop (length akapps) kvs
        pure $ replaceAllTypeVars (instantiatedKinds <> unknownKinds) ak'
      let (aks', _) = unapplyKinds ak'
      tys <- traverse (lift . lift . freshTypeWithKind) $ drop (length axs) aks'
      let a' = foldl' srcTypeApp a tys
          b' = foldl' srcTypeApp b tys
      pure . Canonicalized $ S.singleton (a', b')
  | otherwise = empty

-- | Constraints of the form
-- @Coercible ( label_0 :: a_0, .. label_n :: a_n | r ) ( label_0 :: b_0, .. label_n :: b_n | s )@
-- yield a constraint @Coercible r s@ and constraints on the types for each
-- label in both rows. Labels exclusive to one row yield a failure.
canonRow
  :: SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonRow a b
  | RCons{} <- a =
      case alignRowsWith (const (,)) a b of
        -- We throw early when a bare unknown remains on either side after
        -- aligning the rows because we don't know how to canonicalize them yet
        -- and the unification error thrown when the rows are misaligned should
        -- not mention unknowns.
        (_, (([], u@TUnknown{}), rl2)) -> do
          k <- lift $ lift $ elaborateKind u
          throwError $ insoluble k u (rowFromList rl2)
        (_, (rl1, ([], u@TUnknown{}))) -> do
          k <- lift $ lift $ elaborateKind u
          throwError $ insoluble k (rowFromList rl1) u
        (deriveds, (([], tail1), ([], tail2))) -> do
          pure . Canonicalized . S.fromList $ (tail1, tail2) : deriveds
        (_, (rl1, rl2)) ->
          throwError . errorMessage $ TypesDoNotUnify (rowFromList rl1) (rowFromList rl2)
  | otherwise = empty

-- | Unwrapping a newtype can fails in two ways:
data UnwrapNewtypeError
  = CannotUnwrapInfiniteNewtypeChain
  -- ^ The newtype might wrap an infinite newtype chain. We may think that this
  -- is already handled by the solver depth check, but failing to unwrap
  -- infinite chains of newtypes let us try other rules.
  --
  -- For instance the declarations:
  --
  -- @
  -- newtype N a = N (N a)
  -- type role N representational
  --
  -- example :: forall a b. Coercible a b => N a -> N b
  -- example = coerce
  -- @
  --
  -- yield a wanted @Coercible (N a) (N b)@ that we can decompose to
  -- @Coercible a b@ then discharge with the given if the newtype
  -- unwrapping rules do not apply.
  | CannotUnwrapConstructor
  -- ^ The constructor may not be in scope or may not belong to a newtype.

-- | Unwraps a newtype and yields its underlying type with the newtype arguments
-- substituted in (e.g. @N[D/a] = D@ given @newtype N a = N a@ and @data D = D@).
unwrapNewtype
  :: Environment
  -> SourceType
  -> CanonM (Either UnwrapNewtypeError SourceType)
unwrapNewtype env = go (0 :: Int) where
  go n ty = runExceptT $ do
    when (n > 1000) $ throwError CannotUnwrapInfiniteNewtypeChain
    (currentModuleName, currentModuleImports) <- gets $ checkCurrentModule &&& checkCurrentModuleImports
    case unapplyTypes ty of
      (TypeConstructor _ newtypeName, ks, xs)
        | Just (inScope, fromModuleName, tvs, newtypeCtorName, wrappedTy) <-
            lookupNewtypeConstructorInScope env currentModuleName currentModuleImports newtypeName ks
        -- We refuse to unwrap newtypes over polytypes because we don't know how
        -- to canonicalize them yet and we'd rather try to make progress with
        -- another rule.
        , isMonoType wrappedTy -> do
            unless inScope $ do
              tell [MissingConstructorImportForCoercible newtypeCtorName]
              throwError CannotUnwrapConstructor
            for_ fromModuleName $ flip addConstructorImportForCoercible newtypeCtorName
            let wrappedTySub = replaceAllTypeVars (zip tvs xs) wrappedTy
            ExceptT (go (n + 1) wrappedTySub) `catchError` \case
              CannotUnwrapInfiniteNewtypeChain -> throwError CannotUnwrapInfiniteNewtypeChain
              CannotUnwrapConstructor -> pure wrappedTySub
      _ -> throwError CannotUnwrapConstructor
  addConstructorImportForCoercible fromModuleName newtypeCtorName = modify $ \st ->
    st { checkConstructorImportsForCoercible = S.insert (fromModuleName, newtypeCtorName) $ checkConstructorImportsForCoercible st }

-- | Looks up a given name and, if it names a newtype, returns the names of the
-- type's parameters, the type the newtype wraps and the names of the type's
-- fields.
lookupNewtypeConstructor
  :: Environment
  -> Qualified (ProperName 'TypeName)
  -> [SourceType]
  -> Maybe ([Text], ProperName 'ConstructorName, SourceType)
lookupNewtypeConstructor env qualifiedNewtypeName ks = do
  (newtyk, DataType Newtype tvs [(ctorName, [wrappedTy])]) <- M.lookup qualifiedNewtypeName (types env)
  let (kvs, _) = fromMaybe (internalError "lookupNewtypeConstructor: unkinded forall binder") $ completeBinderList newtyk
      instantiatedKinds = zipWith (\(_, (kv, _)) k -> (kv, k)) kvs ks
  pure (map (\(name, _, _) -> name) tvs, ctorName, replaceAllTypeVars instantiatedKinds wrappedTy)

-- | Behaves like 'lookupNewtypeConstructor' but also returns whether the
-- newtype constructor is in scope and the module from which it is imported, or
-- 'Nothing' if it is defined in the current module.
lookupNewtypeConstructorInScope
  :: Environment
  -> Maybe ModuleName
  -> [ ( SourceAnn
       , ModuleName
       , ImportDeclarationType
       , Maybe ModuleName
       , M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
       )
     ]
  -> Qualified (ProperName 'TypeName)
  -> [SourceType]
  -> Maybe (Bool, Maybe ModuleName, [Text], Qualified (ProperName 'ConstructorName), SourceType)
lookupNewtypeConstructorInScope env currentModuleName currentModuleImports qualifiedNewtypeName@(Qualified newtypeModuleName newtypeName) ks = do
  let fromModule = find isNewtypeCtorImported currentModuleImports
      fromModuleName = (\(_, n, _, _, _) -> n) <$> fromModule
      asModuleName = (\(_, _, _, n, _) -> n) =<< fromModule
      isDefinedInCurrentModule = toMaybeModuleName newtypeModuleName == currentModuleName
      isImported = isJust fromModule
      inScope = isDefinedInCurrentModule || isImported
  (tvs, ctorName, wrappedTy) <- lookupNewtypeConstructor env qualifiedNewtypeName ks
  pure (inScope, fromModuleName, tvs, Qualified (byMaybeModuleName asModuleName) ctorName, wrappedTy)
  where
  isNewtypeCtorImported (_, _, importDeclType, _, exportedTypes) =
    case M.lookup newtypeName exportedTypes of
      Just ([_], _) -> case importDeclType of
        Implicit -> True
        Explicit refs -> any isNewtypeCtorRef refs
        Hiding refs -> not $ any isNewtypeCtorRef refs
      _ -> False
  isNewtypeCtorRef = \case
    TypeRef _ importedTyName Nothing -> importedTyName == newtypeName
    TypeRef _ importedTyName (Just [_]) -> importedTyName == newtypeName
    _ -> False

-- | Constraints of the form @Coercible (N a_0 .. a_n) b@ yield a constraint
-- @Coercible a b@ if unwrapping the newtype yields @a@.
canonNewtypeLeft
  :: Environment
  -> SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonNewtypeLeft env a b =
  lift (unwrapNewtype env a) >>= \case
    Left CannotUnwrapInfiniteNewtypeChain -> empty
    Left CannotUnwrapConstructor -> empty
    Right a' -> pure . Canonicalized $ S.singleton (a', b)

-- | Constraints of the form @Coercible a (N b_0 .. b_n)@ yield a constraint
-- @Coercible a b@ if unwrapping the newtype yields @b@.
canonNewtypeRight
  :: Environment
  -> SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonNewtypeRight env =
  flip $ canonNewtypeLeft env

-- | Decomposes constraints of the form @Coercible (D a_0 .. a_n) (D b_0 .. b_n)@
-- into constraints on their representational arguments, ignoring phantom
-- arguments and failing on unequal nominal arguments.
--
-- For instance given the declarations:
--
-- @
-- data D a b c = D a b
-- type role D nominal representational phantom
-- @
--
-- We can decompose @Coercible (D a b d) (D a c e)@ into @Coercible b c@, but
-- decomposing @Coercible (D a c d) (D b c d)@ would fail.
decompose
  :: Environment
  -> Qualified (ProperName 'TypeName)
  -> [SourceType]
  -> [SourceType]
  -> TypeCheckM Canonicalized
decompose env tyName axs bxs = do
  let roles = lookupRoles env tyName
      f role ax bx = case role of
        Nominal
          -- If we had first-class equality constraints, we'd just
          -- emit one of the form @(a ~ b)@ here and let the solver
          -- recurse. Since we don't we must compare the types at
          -- this point and fail if they don't match. This likely
          -- means there are cases we should be able to handle that
          -- we currently can't, but is at least sound.
          | ax == bx ->
              pure mempty
          | otherwise ->
              throwError . errorMessage $ TypesDoNotUnify ax bx
        Representational ->
          pure $ S.singleton (ax, bx)
        Phantom ->
          pure mempty
  fmap (Canonicalized . fold) $ sequence $ zipWith3 f roles axs bxs

-- | Constraints of the form @Coercible (D a_0 .. a_n) (D b_0 .. b_n)@, where
-- @D@ is not a newtype, yield constraints on their arguments.
canonDecomposition
  :: Environment
  -> SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonDecomposition env a b
  | (TypeConstructor _ aTyName, _, axs) <- unapplyTypes a
  , (TypeConstructor _ bTyName, _, bxs) <- unapplyTypes b
  , aTyName == bTyName
  , Nothing <- lookupNewtypeConstructor env aTyName [] =
      lift $ lift $ decompose env aTyName axs bxs
  | otherwise = empty

-- | Constraints of the form @Coercible (D1 a_0 .. a_n) (D2 b_0 .. b_n)@, where
-- @D1@ and @D2@ are different type constructors and neither of them are
-- newtypes, are insoluble.
canonDecompositionFailure
  :: Environment
  -> SourceType
  -> SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonDecompositionFailure env k a b
  | (TypeConstructor _ aTyName, _, _) <- unapplyTypes a
  , (TypeConstructor _ bTyName, _, _) <- unapplyTypes b
  , aTyName /= bTyName
  , Nothing <- lookupNewtypeConstructor env aTyName []
  , Nothing <- lookupNewtypeConstructor env bTyName [] =
      throwError $ insoluble k a b
  | otherwise = empty

-- | Wanted constraints of the form @Coercible (N a_0 .. a_n) (N b_0 .. b_n)@,
-- where @N@ is a newtype whose constructor is out of scope, yield constraints
-- on their arguments only when no given constraint can discharge them.
--
-- We cannot decompose given constraints because newtypes are not necessarily
-- injective with respect to representational equality.
--
-- For instance given the declaration:
--
-- @
-- newtype Const a b = MkConst a
-- type role Const representational representational
-- @
--
-- Decomposing a given @Coercible (Const a a) (Const a b)@ constraint to
-- @Coercible a b@ when @MkConst@ is out of scope would let us coerce arbitrary
-- types in modules where @MkConst@ is imported, because the given is easily
-- satisfied with the newtype unwrapping rules.
--
-- Moreover we do not decompose wanted constraints if they could be discharged
-- by a given constraint.
--
-- For instance the declaration:
--
-- @
-- example :: forall a b. Coercible (Const a a) (Const a b) => Const a a -> Const a b
-- example = coerce
-- @
--
-- yield an irreducible given @Coercible (Const a a) (Const a b)@ when @MkConst@
-- is out of scope. Would we decompose the wanted
-- @Coercible (Const a a) (Const a b)@ to @Coercible a b@ we would not be able
-- to discharge it with the given.
canonNewtypeDecomposition
  :: Environment
  -> Maybe [(SourceType, SourceType, SourceType)]
  -> SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonNewtypeDecomposition env (Just givens) a b
  | (TypeConstructor _ aTyName, _, axs) <- unapplyTypes a
  , (TypeConstructor _ bTyName, _, bxs) <- unapplyTypes b
  , aTyName == bTyName
  , Just _ <- lookupNewtypeConstructor env aTyName [] = do
      let givensCanDischarge = any (\given -> canDischarge given (a, b)) givens
      guard $ not givensCanDischarge
      lift $ lift $ decompose env aTyName axs bxs
canonNewtypeDecomposition _ _ _ _ = empty

-- | Constraints of the form @Coercible (N1 a_0 .. a_n) (N2 b_0 .. b_n)@, where
-- @N1@ and @N2@ are different type constructors and either of them is a
-- newtype whose constructor is out of scope, are irreducible.
canonNewtypeDecompositionFailure
  :: SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonNewtypeDecompositionFailure a b
  | (TypeConstructor{}, _, _) <- unapplyTypes a
  , (TypeConstructor{}, _, _) <- unapplyTypes b
  = pure Irreducible
  | otherwise = empty

-- | Constraints of the form @Coercible tv1 tv2@ may be irreducibles, but only
-- when the variables are lexicographically ordered. Reordering variables is
-- necessary to prevent loops.
--
-- For instance the declaration:
--
-- @
-- example :: forall a b. Coercible a b => Coercible b a => a -> b
-- example = coerce
-- @
--
-- yields the irreducible givens @Coercible a b@ and @Coercible b a@ which would
-- repeatedly kick each other out the inert set whereas reordering the latter to
-- @Coercible a b@ makes it redundant and let us discharge it.
canonTypeVars
  :: SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonTypeVars a b
  | Skolem _ tv1 _ _ _ <- a
  , Skolem _ tv2 _ _ _ <- b
  , tv2 < tv1
  = pure . Canonicalized $ S.singleton (b, a)
  | Skolem{} <- a, Skolem{} <- b
  = pure Irreducible
  | otherwise = empty

-- | Constraints of the form @Coercible tv ty@ are irreducibles.
canonTypeVarLeft
  :: SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonTypeVarLeft a _
  | Skolem{} <- a = pure Irreducible
  | otherwise = empty

-- | Constraints of the form @Coercible ty tv@ are reordered to
-- @Coercible tv ty@ to satisfy the canonicality requirement of having the type
-- variable on the left.
canonTypeVarRight
  :: SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonTypeVarRight a b
  | Skolem{} <- b = pure . Canonicalized $ S.singleton (b, a)
  | otherwise = empty

-- | Constraints of the form @Coercible (f a_0 .. a_n) b@ are irreducibles.
canonApplicationLeft
  :: SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonApplicationLeft a _
  | TypeApp{} <- a = pure Irreducible
  | otherwise = empty

-- | Constraints of the form @Coercible a (f b_0 .. b_n) b@ are irreducibles.
canonApplicationRight
  :: SourceType
  -> SourceType
  -> MaybeT CanonM Canonicalized
canonApplicationRight _ b
  | TypeApp{} <- b = pure Irreducible
  | otherwise = empty
