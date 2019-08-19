{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Type class entailment
--
module Language.PureScript.TypeChecker.Entailment
  ( InstanceContext
  , SolverOptions(..)
  , replaceTypeClassDictionaries
  , newDictionaries
  , entails
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow (second, (&&&))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State
import Control.Monad.Supply.Class (MonadSupply(..))
import Control.Monad.Writer

import Data.Foldable (for_, fold, toList)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (minimumBy, groupBy, nubBy, sortBy, elem, delete)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (for)
import Data.Text (Text, stripPrefix, stripSuffix)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NEL

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types
import Language.PureScript.Label (Label(..))
import Language.PureScript.PSString (PSString, mkString, decodeString)
import qualified Language.PureScript.Constants as C

-- | Describes what sort of dictionary to generate for type class instances
data Evidence
  -- | An existing named instance
  = NamedInstance (Qualified Ident)

  -- | Computed instances
  | WarnInstance SourceType -- ^ Warn type class with a user-defined warning message
  | IsSymbolInstance PSString -- ^ The IsSymbol type class for a given Symbol literal
  | EmptyClassInstance        -- ^ For any solved type class with no members
  deriving (Show, Eq)

-- | Extract the identifier of a named instance
namedInstanceIdentifier :: Evidence -> Maybe (Qualified Ident)
namedInstanceIdentifier (NamedInstance i) = Just i
namedInstanceIdentifier _ = Nothing

-- | Description of a type class dictionary with instance evidence
type TypeClassDict = TypeClassDictionaryInScope Evidence

-- | The 'InstanceContext' tracks those constraints which can be satisfied.
type InstanceContext = M.Map (Maybe ModuleName)
                         (M.Map (Qualified (ProperName 'ClassName))
                           (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict)))

-- | A type substitution which makes an instance head match a list of types.
--
-- Note: we store many types per type variable name. For any name, all types
-- should unify if we are going to commit to an instance.
type Matching a = M.Map Text a

combineContexts :: InstanceContext -> InstanceContext -> InstanceContext
combineContexts = M.unionWith (M.unionWith (M.unionWith (<>)))

-- | Replace type class dictionary placeholders with inferred type class dictionaries
replaceTypeClassDictionaries
  :: forall m
   . (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadSupply m)
  => Bool
  -> Expr
  -> m (Expr, [(Ident, InstanceContext, SourceConstraint)])
replaceTypeClassDictionaries shouldGeneralize expr = flip evalStateT M.empty $ do
    -- Loop, deferring any unsolved constraints, until there are no more
    -- constraints which can be solved, then make a generalization pass.
    let loop e = do
          (e', solved) <- deferPass e
          if getAny solved
            then loop e'
            else return e'
    loop expr >>= generalizePass
  where
    -- This pass solves constraints where possible, deferring constraints if not.
    deferPass :: Expr -> StateT InstanceContext m (Expr, Any)
    deferPass = fmap (second fst) . runWriterT . f where
      f :: Expr -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext m) Expr
      (_, f, _) = everywhereOnValuesTopDownM return (go True) return

    -- This pass generalizes any remaining constraints
    generalizePass :: Expr -> StateT InstanceContext m (Expr, [(Ident, InstanceContext, SourceConstraint)])
    generalizePass = fmap (second snd) . runWriterT . f where
      f :: Expr -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext m) Expr
      (_, f, _) = everywhereOnValuesTopDownM return (go False) return

    go :: Bool -> Expr -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext m) Expr
    go deferErrors (TypeClassDictionary constraint context hints) =
      rethrow (addHints hints) $ entails (SolverOptions shouldGeneralize deferErrors) constraint context hints
    go _ other = return other

-- | Three options for how we can handle a constraint, depending on the mode we're in.
data EntailsResult a
  = Solved a TypeClassDict
  -- ^ We solved this constraint
  | Unsolved SourceConstraint
  -- ^ We couldn't solve this constraint right now, it will be generalized
  | Deferred
  -- ^ We couldn't solve this constraint right now, so it has been deferred
  deriving Show

-- | Options for the constraint solver
data SolverOptions = SolverOptions
  { solverShouldGeneralize :: Bool
  -- ^ Should the solver be allowed to generalize over unsolved constraints?
  , solverDeferErrors      :: Bool
  -- ^ Should the solver be allowed to defer errors by skipping constraints?
  }

data Matched t
  = Match t
  | Apart
  | Unknown
  deriving (Eq, Show, Functor)

instance Semigroup t => Semigroup (Matched t) where
  (Match l) <> (Match r) = Match (l <> r)
  Apart     <> _         = Apart
  _         <> Apart     = Apart
  _         <> _         = Unknown

instance Monoid t => Monoid (Matched t) where
  mempty = Match mempty

-- | Check that the current set of type class dictionaries entail the specified type class goal, and, if so,
-- return a type class dictionary reference.
entails
  :: forall m
   . (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m, MonadSupply m)
  => SolverOptions
  -- ^ Solver options
  -> SourceConstraint
  -- ^ The constraint to solve
  -> InstanceContext
  -- ^ The contexts in which to solve the constraint
  -> [ErrorMessageHint]
  -- ^ Error message hints to apply to any instance errors
  -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext m) Expr
entails SolverOptions{..} constraint context hints =
    solve constraint
  where
    forClassName :: InstanceContext -> Qualified (ProperName 'ClassName) -> [SourceType] -> [TypeClassDict]
    forClassName ctx cn@C.Warn [msg] =
      -- Prefer a warning dictionary in scope if there is one available.
      -- This allows us to defer a warning by propagating the constraint.
      findDicts ctx cn Nothing ++ [TypeClassDictionaryInScope [] 0 (WarnInstance msg) [] C.Warn [msg] Nothing]
    forClassName _ C.IsSymbol args | Just dicts <- solveIsSymbol args = dicts
    forClassName _ C.SymbolCompare args | Just dicts <- solveSymbolCompare args = dicts
    forClassName _ C.SymbolAppend args | Just dicts <- solveSymbolAppend args = dicts
    forClassName _ C.SymbolCons args | Just dicts <- solveSymbolCons args = dicts
    forClassName _ C.RowUnion args | Just dicts <- solveUnion args = dicts
    forClassName _ C.RowNub args | Just dicts <- solveNub args = dicts
    forClassName _ C.RowLacks args | Just dicts <- solveLacks args = dicts
    forClassName _ C.RowCons args | Just dicts <- solveRowCons args = dicts
    forClassName _ C.RowToList args | Just dicts <- solveRowToList args = dicts
    forClassName ctx cn@(Qualified (Just mn) _) tys = concatMap (findDicts ctx cn) (ordNub (Nothing : Just mn : map Just (mapMaybe ctorModules tys)))
    forClassName _ _ _ = internalError "forClassName: expected qualified class name"

    ctorModules :: SourceType -> Maybe ModuleName
    ctorModules (TypeConstructor _ (Qualified (Just mn) _)) = Just mn
    ctorModules (TypeConstructor _ (Qualified Nothing _)) = internalError "ctorModules: unqualified type name"
    ctorModules (TypeApp _ ty _) = ctorModules ty
    ctorModules (KindedType _ ty _) = ctorModules ty
    ctorModules _ = Nothing

    findDicts :: InstanceContext -> Qualified (ProperName 'ClassName) -> Maybe ModuleName -> [TypeClassDict]
    findDicts ctx cn = fmap (fmap NamedInstance) . foldMap NEL.toList . foldMap M.elems . (>>= M.lookup cn) . flip M.lookup ctx

    valUndefined :: Expr
    valUndefined = Var nullSourceSpan (Qualified (Just (ModuleName [ProperName C.prim])) (Ident C.undefined))

    solve :: SourceConstraint -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext m) Expr
    solve con = go 0 con
      where
        go :: Int -> SourceConstraint -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext m) Expr
        go work (Constraint _ className' tys' _) | work > 1000 = throwError . errorMessage $ PossiblyInfiniteInstance className' tys'
        go work con'@(Constraint _ className' tys' conInfo) = WriterT . StateT . (withErrorMessageHint (ErrorSolvingConstraint con') .) . runStateT . runWriterT $ do
            -- We might have unified types by solving other constraints, so we need to
            -- apply the latest substitution.
            latestSubst <- lift . lift $ gets checkSubstitution
            let tys'' = map (substituteType latestSubst) tys'
            -- Get the inferred constraint context so far, and merge it with the global context
            inferred <- lift get
            -- We need information about functional dependencies, so we have to look up the class
            -- name in the environment:
            classesInScope <- lift . lift $ gets (typeClasses . checkEnv)

            TypeClassData
              { typeClassDependencies
              , typeClassIsEmpty
              } <- case M.lookup className' classesInScope of
                Nothing -> throwError . errorMessage $ UnknownClass className'
                Just tcd -> pure tcd

            let instances = do
                  chain <- groupBy ((==) `on` tcdChain) $
                           sortBy (compare `on` (tcdChain &&& tcdIndex)) $
                           forClassName (combineContexts context inferred) className' tys''
                  -- process instances in a chain in index order
                  let found = for chain $ \tcd ->
                                -- Make sure the type unifies with the type in the type instance definition
                                case matches typeClassDependencies tcd tys'' of
                                  Apart        -> Right ()                  -- keep searching
                                  Match substs -> Left (Just (substs, tcd)) -- found a match
                                  Unknown      -> Left Nothing              -- can't continue with this chain yet, need proof of apartness
                  case found of
                    Right _               -> []          -- all apart
                    Left Nothing          -> []          -- last unknown
                    Left (Just substsTcd) -> [substsTcd] -- found a match
            solution <- lift . lift $ unique tys'' instances
            case solution of
              Solved substs tcd -> do
                -- Note that we solved something.
                tell (Any True, mempty)
                -- Make sure the substitution is valid:
                lift . lift . for_ substs $ pairwiseM unifyTypes
                -- Now enforce any functional dependencies, using unification
                -- Note: we need to generate fresh types for any unconstrained
                -- type variables before unifying.
                let subst = fmap head substs
                currentSubst <- lift . lift $ gets checkSubstitution
                subst' <- lift . lift $ withFreshTypes tcd (fmap (substituteType currentSubst) subst)
                lift . lift $ zipWithM_ (\t1 t2 -> do
                  let inferredType = replaceAllTypeVars (M.toList subst') t1
                  unifyTypes inferredType t2) (tcdInstanceTypes tcd) tys''
                currentSubst' <- lift . lift $ gets checkSubstitution
                let subst'' = fmap (substituteType currentSubst') subst'
                -- Solve any necessary subgoals
                args <- solveSubgoals subst'' (tcdDependencies tcd)

                initDict <- lift . lift $ mkDictionary (tcdValue tcd) args

                let match = foldr (\(className, index) dict -> subclassDictionaryValue dict className index)
                                  initDict
                                  (tcdPath tcd)

                return (if typeClassIsEmpty then Unused match else match)
              Unsolved unsolved -> do
                -- Generate a fresh name for the unsolved constraint's new dictionary
                ident <- freshIdent ("dict" <> runProperName (disqualify (constraintClass unsolved)))
                let qident = Qualified Nothing ident
                -- Store the new dictionary in the InstanceContext so that we can solve this goal in
                -- future.
                newDicts <- lift . lift $ newDictionaries [] qident unsolved
                let newContext = mkContext newDicts
                modify (combineContexts newContext)
                -- Mark this constraint for generalization
                tell (mempty, [(ident, context, unsolved)])
                return (Var nullSourceSpan qident)
              Deferred ->
                -- Constraint was deferred, just return the dictionary unchanged,
                -- with no unsolved constraints. Hopefully, we can solve this later.
                return (TypeClassDictionary (srcConstraint className' tys'' conInfo) context hints)
          where
            -- | When checking functional dependencies, we need to use unification to make
            -- sure it is safe to use the selected instance. We will unify the solved type with
            -- the type in the instance head under the substition inferred from its instantiation.
            -- As an example, when solving MonadState t0 (State Int), we choose the
            -- MonadState s (State s) instance, and we unify t0 with Int, since the functional
            -- dependency from MonadState dictates that t0 should unify with s\[s -> Int], which is
            -- Int. This is fine, but in some cases, the substitution does not remove all TypeVars
            -- from the type, so we end up with a unification error. So, any type arguments which
            -- appear in the instance head, but not in the substitution need to be replaced with
            -- fresh type variables. This function extends a substitution with fresh type variables
            -- as necessary, based on the types in the instance head.
            withFreshTypes
              :: TypeClassDict
              -> Matching SourceType
              -> m (Matching SourceType)
            withFreshTypes TypeClassDictionaryInScope{..} subst = do
                let onType = everythingOnTypes S.union fromTypeVar
                    typeVarsInHead = foldMap onType tcdInstanceTypes
                                  <> foldMap (foldMap (foldMap onType . constraintArgs)) tcdDependencies
                    typeVarsInSubst = S.fromList (M.keys subst)
                    uninstantiatedTypeVars = typeVarsInHead S.\\ typeVarsInSubst
                newSubst <- traverse withFreshType (S.toList uninstantiatedTypeVars)
                return (subst <> M.fromList newSubst)
              where
                fromTypeVar (TypeVar _ v) = S.singleton v
                fromTypeVar _ = S.empty

                withFreshType s = do
                  t <- freshType
                  return (s, t)

            unique :: [SourceType] -> [(a, TypeClassDict)] -> m (EntailsResult a)
            unique tyArgs []
              | solverDeferErrors = return Deferred
              -- We need a special case for nullary type classes, since we want
              -- to generalize over Partial constraints.
              | solverShouldGeneralize && (null tyArgs || any canBeGeneralized tyArgs) = return (Unsolved (srcConstraint className' tyArgs conInfo))
              | otherwise = throwError . errorMessage $ NoInstanceFound (srcConstraint className' tyArgs conInfo)
            unique _      [(a, dict)] = return $ Solved a dict
            unique tyArgs tcds
              | pairwiseAny overlapping (map snd tcds) =
                  throwError . errorMessage $ OverlappingInstances className' tyArgs (tcds >>= (toList . namedInstanceIdentifier . tcdValue . snd))
              | otherwise = return $ uncurry Solved (minimumBy (compare `on` length . tcdPath . snd) tcds)

            canBeGeneralized :: Type a -> Bool
            canBeGeneralized TUnknown{} = True
            canBeGeneralized (KindedType _ t _) = canBeGeneralized t
            canBeGeneralized _ = False

            -- |
            -- Check if two dictionaries are overlapping
            --
            -- Dictionaries which are subclass dictionaries cannot overlap, since otherwise the overlap would have
            -- been caught when constructing superclass dictionaries.
            overlapping :: TypeClassDict -> TypeClassDict -> Bool
            overlapping TypeClassDictionaryInScope{ tcdPath = _ : _ } _ = False
            overlapping _ TypeClassDictionaryInScope{ tcdPath = _ : _ } = False
            overlapping TypeClassDictionaryInScope{ tcdDependencies = Nothing } _ = False
            overlapping _ TypeClassDictionaryInScope{ tcdDependencies = Nothing } = False
            overlapping tcd1 tcd2 = tcdValue tcd1 /= tcdValue tcd2

            -- Create dictionaries for subgoals which still need to be solved by calling go recursively
            -- E.g. the goal (Show a, Show b) => Show (Either a b) can be satisfied if the current type
            -- unifies with Either a b, and we can satisfy the subgoals Show a and Show b recursively.
            solveSubgoals :: Matching SourceType -> Maybe [SourceConstraint] -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext m) (Maybe [Expr])
            solveSubgoals _ Nothing = return Nothing
            solveSubgoals subst (Just subgoals) =
              Just <$> traverse (go (work + 1) . mapConstraintArgs (map (replaceAllTypeVars (M.toList subst)))) subgoals

            -- We need subgoal dictionaries to appear in the term somewhere
            -- If there aren't any then the dictionary is just undefined
            useEmptyDict :: Maybe [Expr] -> Expr
            useEmptyDict args = Unused (foldl (App . Abs (VarBinder nullSourceSpan UnusedIdent)) valUndefined (fold args))

            -- Make a dictionary from subgoal dictionaries by applying the correct function
            mkDictionary :: Evidence -> Maybe [Expr] -> m Expr
            mkDictionary (NamedInstance n) args = return $ foldl App (Var nullSourceSpan n) (fold args)
            mkDictionary EmptyClassInstance args = return (useEmptyDict args)
            mkDictionary (WarnInstance msg) args = do
              tell . errorMessage $ UserDefinedWarning msg
              -- We cannot call the type class constructor here because Warn is declared in Prim.
              -- This means that it doesn't have a definition that we can import.
              -- So pass an empty placeholder (undefined) instead.
              return (useEmptyDict args)
            mkDictionary (IsSymbolInstance sym) _ =
              let fields = [ ("reflectSymbol", Abs (VarBinder nullSourceSpan UnusedIdent) (Literal nullSourceSpan (StringLiteral sym))) ] in
              return $ TypeClassDictionaryConstructorApp C.IsSymbol (Literal nullSourceSpan (ObjectLiteral fields))

        -- Turn a DictionaryValue into a Expr
        subclassDictionaryValue :: Expr -> Qualified (ProperName 'ClassName) -> Integer -> Expr
        subclassDictionaryValue dict className index =
          App (Accessor (mkString (superclassName className index)) dict) valUndefined

    solveIsSymbol :: [SourceType] -> Maybe [TypeClassDict]
    solveIsSymbol [TypeLevelString ann sym] = Just [TypeClassDictionaryInScope [] 0 (IsSymbolInstance sym) [] C.IsSymbol [TypeLevelString ann sym] Nothing]
    solveIsSymbol _ = Nothing

    solveSymbolCompare :: [SourceType] -> Maybe [TypeClassDict]
    solveSymbolCompare [arg0@(TypeLevelString _ lhs), arg1@(TypeLevelString _ rhs), _] =
      let ordering = case compare lhs rhs of
                  LT -> C.orderingLT
                  EQ -> C.orderingEQ
                  GT -> C.orderingGT
          args' = [arg0, arg1, srcTypeConstructor ordering]
      in Just [TypeClassDictionaryInScope [] 0 EmptyClassInstance [] C.SymbolCompare args' Nothing]
    solveSymbolCompare _ = Nothing

    solveSymbolAppend :: [SourceType] -> Maybe [TypeClassDict]
    solveSymbolAppend [arg0, arg1, arg2] = do
      (arg0', arg1', arg2') <- appendSymbols arg0 arg1 arg2
      let args' = [arg0', arg1', arg2']
      pure [TypeClassDictionaryInScope [] 0 EmptyClassInstance [] C.SymbolAppend args' Nothing]
    solveSymbolAppend _ = Nothing

    -- | Append type level symbols, or, run backwards, strip a prefix or suffix
    appendSymbols :: SourceType -> SourceType -> SourceType -> Maybe (SourceType, SourceType, SourceType)
    appendSymbols arg0@(TypeLevelString _ lhs) arg1@(TypeLevelString _ rhs) _ = Just (arg0, arg1, srcTypeLevelString (lhs <> rhs))
    appendSymbols arg0@(TypeLevelString _ lhs) _ arg2@(TypeLevelString _ out) = do
      lhs' <- decodeString lhs
      out' <- decodeString out
      rhs <- stripPrefix lhs' out'
      pure (arg0, srcTypeLevelString (mkString rhs), arg2)
    appendSymbols _ arg1@(TypeLevelString _ rhs) arg2@(TypeLevelString _ out) = do
      rhs' <- decodeString rhs
      out' <- decodeString out
      lhs <- stripSuffix rhs' out'
      pure (srcTypeLevelString (mkString lhs), arg1, arg2)
    appendSymbols _ _ _ = Nothing

    solveSymbolCons :: [SourceType] -> Maybe [TypeClassDict]
    solveSymbolCons [arg0, arg1, arg2] = do
      (arg0', arg1', arg2') <- consSymbol arg0 arg1 arg2
      let args' = [arg0', arg1', arg2']
      pure [TypeClassDictionaryInScope [] 0 EmptyClassInstance [] C.SymbolCons args' Nothing]
    solveSymbolCons _ = Nothing

    consSymbol :: SourceType -> SourceType -> SourceType -> Maybe (SourceType, SourceType, SourceType)
    consSymbol _ _ arg@(TypeLevelString _ s) = do
      (h, t) <- T.uncons =<< decodeString s
      pure (mkTLString (T.singleton h), mkTLString t, arg)
      where mkTLString = srcTypeLevelString . mkString
    consSymbol arg1@(TypeLevelString _ h) arg2@(TypeLevelString _ t) _ = do
      h' <- decodeString h
      t' <- decodeString t
      guard (T.length h' == 1)
      pure (arg1, arg2, srcTypeLevelString (mkString $ h' <> t'))
    consSymbol _ _ _ = Nothing

    solveUnion :: [SourceType] -> Maybe [TypeClassDict]
    solveUnion [l, r, u] = do
      (lOut, rOut, uOut, cst) <- unionRows l r u
      pure [ TypeClassDictionaryInScope [] 0 EmptyClassInstance [] C.RowUnion [lOut, rOut, uOut] cst ]
    solveUnion _ = Nothing

    -- | Left biased union of two row types
    unionRows :: SourceType -> SourceType -> SourceType -> Maybe (SourceType, SourceType, SourceType, Maybe [SourceConstraint])
    unionRows l r u = result
      where
        (fixed, rest) = rowToList l

        rowVar = srcTypeVar "r"

        result =
          case rest of
            -- If the left hand side is a closed row, then we can merge
            -- its labels into the right hand side.
            REmpty _ -> Just (l, r, rowFromList (fixed, r), Nothing)
            -- If the right hand side and output are closed rows, then we can
            -- compute the left hand side by subtracting the right hand side
            -- from the output.
            _ | (right, REmpty _) <- rowToList r
              , (output, restu@(REmpty _)) <- rowToList u ->
              let
                -- Partition the output rows into those that belong in right
                -- (taken off the end) and those that must end up in left.
                grabLabel e (left', right', remaining)
                  | rowListLabel e `elem` remaining =
                    (left', e : right', delete (rowListLabel e) remaining)
                  | otherwise =
                    (e : left', right', remaining)
                (outL, outR, leftover) =
                  foldr grabLabel ([], [], fmap rowListLabel right) output
              in guard (null leftover) $>
                (rowFromList (outL, restu), rowFromList (outR, restu), u, Nothing)
            -- If the left hand side is not definitely closed, then the only way we
            -- can safely make progress is to move any known labels from the left
            -- input into the output, and add a constraint for any remaining labels.
            -- Otherwise, the left hand tail might contain the same labels as on
            -- the right hand side, and we can't be certain we won't reorder the
            -- types for such labels.
            _ -> guard (not (null fixed)) $>
              (l, r, rowFromList (fixed, rowVar), Just [ srcConstraint C.RowUnion [rest, r, rowVar] Nothing ])

    solveRowCons :: [SourceType] -> Maybe [TypeClassDict]
    solveRowCons [TypeLevelString ann sym, ty, r, _] =
      Just [ TypeClassDictionaryInScope [] 0 EmptyClassInstance [] C.RowCons [TypeLevelString ann sym, ty, r, srcRCons (Label sym) ty r] Nothing ]
    solveRowCons _ = Nothing

    solveRowToList :: [SourceType] -> Maybe [TypeClassDict]
    solveRowToList [r, _] = do
      entries <- rowToRowList r
      pure [ TypeClassDictionaryInScope [] 0 EmptyClassInstance [] C.RowToList [r, entries] Nothing ]
    solveRowToList _ = Nothing

    -- | Convert a closed row to a sorted list of entries
    rowToRowList :: SourceType -> Maybe SourceType
    rowToRowList r =
        guard (eqType rest $ REmpty ()) $>
        foldr rowListCons (srcTypeConstructor C.RowListNil) fixed
      where
        (fixed, rest) = rowToSortedList r
        rowListCons (RowListItem _ lbl ty) tl =
          foldl srcTypeApp (srcTypeConstructor C.RowListCons)
            [ srcTypeLevelString (runLabel lbl)
            , ty
            , tl ]

    solveNub :: [SourceType] -> Maybe [TypeClassDict]
    solveNub [r, _] = do
      r' <- nubRows r
      pure [ TypeClassDictionaryInScope [] 0 EmptyClassInstance [] C.RowNub [r, r'] Nothing ]
    solveNub _ = Nothing

    nubRows :: SourceType -> Maybe SourceType
    nubRows r =
        guard (eqType rest $ REmpty ()) $>
        rowFromList (nubBy ((==) `on` rowListLabel) fixed, rest)
      where
        (fixed, rest) = rowToSortedList r

    solveLacks :: [SourceType] -> Maybe [TypeClassDict]
    solveLacks [TypeLevelString ann sym, r] = do
      (r', cst) <- rowLacks sym r
      pure [ TypeClassDictionaryInScope [] 0 EmptyClassInstance [] C.RowLacks [TypeLevelString ann sym, r'] cst ]
    solveLacks _ = Nothing

    rowLacks :: PSString -> SourceType -> Maybe (SourceType, Maybe [SourceConstraint])
    rowLacks sym r =
        guard (lacksSym && canMakeProgress) $> (r, cst)
      where
        (fixed, rest) = rowToList r

        lacksSym =
          not $ sym `elem` (runLabel . rowListLabel <$> fixed)

        (canMakeProgress, cst) = case rest of
            REmpty _ -> (True, Nothing)
            _ -> (not (null fixed), Just [ srcConstraint C.RowLacks [srcTypeLevelString sym, rest] Nothing ])

-- Check if an instance matches our list of types, allowing for types
-- to be solved via functional dependencies. If the types match, we return a
-- substitution which makes them match. If not, we return 'Nothing'.
matches :: [FunctionalDependency] -> TypeClassDict -> [SourceType] -> Matched (Matching [SourceType])
matches deps TypeClassDictionaryInScope{..} tys =
    -- First, find those types which match exactly
    let matched = zipWith typeHeadsAreEqual tys tcdInstanceTypes in
    -- Now, use any functional dependencies to infer any remaining types
    if not (covers matched)
       then if any ((==) Apart . fst) matched then Apart else Unknown
       else -- Verify that any repeated type variables are unifiable
            let determinedSet = foldMap (S.fromList . fdDetermined) deps
                solved = map snd . filter ((`S.notMember` determinedSet) . fst) $ zipWith (\(_, ts) i -> (i, ts)) matched [0..]
            in verifySubstitution (M.unionsWith (++) solved)
  where
    -- | Find the closure of a set of functional dependencies.
    covers :: [(Matched (), subst)] -> Bool
    covers ms = finalSet == S.fromList [0..length ms - 1]
      where
        initialSet :: S.Set Int
        initialSet = S.fromList . map snd . filter ((==) (Match ()) . fst . fst) $ zip ms [0..]

        finalSet :: S.Set Int
        finalSet = untilFixedPoint applyAll initialSet

        untilFixedPoint :: Eq a => (a -> a) -> a -> a
        untilFixedPoint f = go
          where
          go a | a' == a = a'
               | otherwise = go a'
            where a' = f a

        applyAll :: S.Set Int -> S.Set Int
        applyAll s = foldr applyDependency s deps

        applyDependency :: FunctionalDependency -> S.Set Int -> S.Set Int
        applyDependency FunctionalDependency{..} xs
          | S.fromList fdDeterminers `S.isSubsetOf` xs = xs <> S.fromList fdDetermined
          | otherwise = xs

    --
    -- Check whether the type heads of two types are equal (for the purposes of type class dictionary lookup),
    -- and return a substitution from type variables to types which makes the type heads unify.
    --
    typeHeadsAreEqual :: Type a -> Type a -> (Matched (), Matching [Type a])
    typeHeadsAreEqual (KindedType _  t1 _) t2                                  = typeHeadsAreEqual t1 t2
    typeHeadsAreEqual t1                     (KindedType _ t2 _)               = typeHeadsAreEqual t1 t2
    typeHeadsAreEqual (TUnknown _ u1)        (TUnknown _ u2)      | u1 == u2   = (Match (), M.empty)
    typeHeadsAreEqual (Skolem _ _ s1 _)      (Skolem _ _ s2 _)    | s1 == s2   = (Match (), M.empty)
    typeHeadsAreEqual t                      (TypeVar _ v)                     = (Match (), M.singleton v [t])
    typeHeadsAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = (Match (), M.empty)
    typeHeadsAreEqual (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = (Match (), M.empty)
    typeHeadsAreEqual (TypeApp _ h1 t1)      (TypeApp _ h2 t2)                 =
      both (typeHeadsAreEqual h1 h2) (typeHeadsAreEqual t1 t2)
    typeHeadsAreEqual (REmpty _) (REmpty _) = (Match (), M.empty)
    typeHeadsAreEqual r1@RCons{} r2@RCons{} =
        foldr both (uncurry go rest) common
      where
        (common, rest) = alignRowsWith typeHeadsAreEqual r1 r2

        go :: ([RowListItem a], Type a) -> ([RowListItem a], Type a) -> (Matched (), Matching [Type a])
        go (l,  KindedType _ t1 _) (r,  t2)                            = go (l, t1) (r, t2)
        go (l,  t1)                (r,  KindedType _ t2 _)             = go (l, t1) (r, t2)
        go ([], REmpty _)          ([], REmpty _)                      = (Match (), M.empty)
        go ([], TUnknown _ u1)     ([], TUnknown _ u2)      | u1 == u2 = (Match (), M.empty)
        go ([], TypeVar _ v1)      ([], TypeVar _ v2)       | v1 == v2 = (Match (), M.empty)
        go ([], Skolem _ _ sk1 _)  ([], Skolem _ _ sk2 _) | sk1 == sk2 = (Match (), M.empty)
        go ([], TUnknown _ _)      _                                   = (Unknown, M.empty)
        go (sd, r)                 ([], TypeVar _ v)                   = (Match (), M.singleton v [rowFromList (sd, r)])
        go _ _                                                         = (Apart, M.empty)
    typeHeadsAreEqual (TUnknown _ _) _ = (Unknown, M.empty)
    typeHeadsAreEqual _ _ = (Apart, M.empty)


    both :: (Matched (), Matching [Type a]) -> (Matched (), Matching [Type a]) -> (Matched (), Matching [Type a])
    both (b1, m1) (b2, m2) = (b1 <> b2, M.unionWith (++) m1 m2)

    -- Ensure that a substitution is valid
    verifySubstitution :: Matching [Type a] -> Matched (Matching [Type a])
    verifySubstitution mts = foldMap meet mts $> mts where
      meet = pairwiseAll typesAreEqual

      -- Note that unknowns are only allowed to unify if they came from a type
      -- which was _not_ solved, i.e. one which was inferred by a functional
      -- dependency.
      typesAreEqual :: Type a -> Type a -> Matched ()
      typesAreEqual (KindedType _ t1 _)    t2                     = typesAreEqual t1 t2
      typesAreEqual t1                     (KindedType _ t2 _)    = typesAreEqual t1 t2
      typesAreEqual (TUnknown _ u1)        (TUnknown _ u2)        | u1 == u2 = Match ()
      typesAreEqual (Skolem _ _ s1 _)      (Skolem _ _ s2 _)      | s1 == s2 = Match ()
      typesAreEqual (Skolem _ _ _ _)       _                      = Unknown
      typesAreEqual _                      (Skolem _ _ _ _)       = Unknown
      typesAreEqual (TypeVar _ v1)         (TypeVar _ v2)         | v1 == v2 = Match ()
      typesAreEqual (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = Match ()
      typesAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = Match ()
      typesAreEqual (TypeApp _ h1 t1)      (TypeApp _ h2 t2)      = typesAreEqual h1 h2 <> typesAreEqual t1 t2
      typesAreEqual (REmpty _)             (REmpty _)             = Match ()
      typesAreEqual r1                     r2                     | isRCons r1 || isRCons r2 =
          let (common, rest) = alignRowsWith typesAreEqual r1 r2
          in fold common <> uncurry go rest
        where
          go :: ([RowListItem a], Type a) -> ([RowListItem a], Type a) -> Matched ()
          go (l, KindedType _ t1 _) (r, t2)                           = go (l, t1) (r, t2)
          go (l, t1)                (r, KindedType _ t2 _)            = go (l, t1) (r, t2)
          go ([], TUnknown _ u1)    ([], TUnknown _ u2)    | u1 == u2 = Match ()
          go ([], Skolem _ _ s1 _)  ([], Skolem _ _ s2 _)  | s1 == s2 = Match ()
          go ([], Skolem _ _ _ _)   _                                 = Unknown
          go _                      ([], Skolem _ _ _ _)              = Unknown
          go ([], REmpty _)         ([], REmpty _)                    = Match ()
          go ([], TypeVar _ v1)     ([], TypeVar _ v2)     | v1 == v2 = Match ()
          go _  _                                                     = Apart
      typesAreEqual _               _                                 = Apart

      isRCons :: Type a -> Bool
      isRCons RCons{}    = True
      isRCons _          = False

-- | Add a dictionary for the constraint to the scope, and dictionaries
-- for all implied superclass instances.
newDictionaries
  :: MonadState CheckState m
  => [(Qualified (ProperName 'ClassName), Integer)]
  -> Qualified Ident
  -> SourceConstraint
  -> m [NamedDict]
newDictionaries path name (Constraint _ className instanceTy _) = do
    tcs <- gets (typeClasses . checkEnv)
    let TypeClassData{..} = fromMaybe (internalError "newDictionaries: type class lookup failed") $ M.lookup className tcs
    supDicts <- join <$> zipWithM (\(Constraint ann supName supArgs _) index ->
                                      newDictionaries ((supName, index) : path)
                                                      name
                                                      (Constraint ann supName (instantiateSuperclass (map fst typeClassArguments) supArgs instanceTy) Nothing)
                                  ) typeClassSuperclasses [0..]
    return (TypeClassDictionaryInScope [] 0 name path className instanceTy Nothing : supDicts)
  where
    instantiateSuperclass :: [Text] -> [SourceType] -> [SourceType] -> [SourceType]
    instantiateSuperclass args supArgs tys = map (replaceAllTypeVars (zip args tys)) supArgs

mkContext :: [NamedDict] -> InstanceContext
mkContext = foldr combineContexts M.empty . map fromDict where
  fromDict d = M.singleton Nothing (M.singleton (tcdClassName d) (M.singleton (tcdValue d) (pure d)))

-- | Check all pairs of values in a list match a predicate
pairwiseAll :: Monoid m => (a -> a -> m) -> [a] -> m
pairwiseAll _ [] = mempty
pairwiseAll _ [_] = mempty
pairwiseAll p (x : xs) = foldMap (p x) xs <> pairwiseAll p xs

-- | Check any pair of values in a list match a predicate
pairwiseAny :: (a -> a -> Bool) -> [a] -> Bool
pairwiseAny _ [] = False
pairwiseAny _ [_] = False
pairwiseAny p (x : xs) = any (p x) xs || pairwiseAny p xs

pairwiseM :: Applicative m => (a -> a -> m ()) -> [a] -> m ()
pairwiseM _ [] = pure ()
pairwiseM _ [_] = pure ()
pairwiseM p (x : xs) = traverse (p x) xs *> pairwiseM p xs
