-- |
-- Type class entailment
--
module Language.PureScript.TypeChecker.Entailment
  ( InstanceContext
  , SolverOptions(..)
  , replaceTypeClassDictionaries
  , newDictionaries
  , entails
  , findDicts
  ) where

import Prelude
import Protolude (ordNub, headMay)

import Control.Arrow (second, (&&&))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State (MonadState(..), MonadTrans(..), StateT(..), evalStateT, execStateT, gets, modify)
import Control.Monad (foldM, guard, join, zipWithM, zipWithM_, (<=<))
import Control.Monad.Writer (MonadWriter(..), WriterT(..))
import Data.Monoid (Any(..))

import Data.Either (lefts, partitionEithers)
import Data.Foldable (for_, fold, toList)
import Data.Function (on)
import Data.Functor (($>), (<&>))
import Data.List (delete, findIndices, minimumBy, nubBy, sortOn, tails)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Traversable (for)
import Data.Text (Text, stripPrefix, stripSuffix)
import Data.Text qualified as T
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NEL

import Language.PureScript.AST (Binder(..), ErrorMessageHint(..), Expr(..), Literal(..), pattern NullSourceSpan, everywhereOnValuesTopDownM, nullSourceSpan, everythingOnValues)
import Language.PureScript.AST.Declarations (UnknownsHint(..))
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (Environment(..), FunctionalDependency(..), TypeClassData(..), dictTypeName, kindRow, tyBoolean, tyInt, tyString)
import Language.PureScript.Errors (SimpleErrorMessage(..), addHint, addHints, errorMessage, rethrow)
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), byMaybeModuleName, coerceProperName, disqualify, freshIdent, getQual)
import Language.PureScript.TypeChecker.Entailment.Coercible (GivenSolverState(..), WantedSolverState(..), initialGivenSolverState, initialWantedSolverState, insoluble, solveGivens, solveWanteds)
import Language.PureScript.TypeChecker.Entailment.IntCompare (mkFacts, mkRelation, solveRelation)
import Language.PureScript.TypeChecker.Kinds (elaborateKind, unifyKinds')
import Language.PureScript.TypeChecker.Monad (CheckState(..), withErrorMessageHint, TypeCheckM)
import Language.PureScript.TypeChecker.Synonyms (replaceAllTypeSynonyms)
import Language.PureScript.TypeChecker.Unify (freshTypeWithKind, substituteType, unifyTypes)
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..), superclassName)
import Language.PureScript.Types
import Language.PureScript.Label (Label(..))
import Language.PureScript.PSString (PSString, mkString, decodeString)
import Language.PureScript.Constants.Libs qualified as C
import Language.PureScript.Constants.Prim qualified as C

-- | Describes what sort of dictionary to generate for type class instances
data Evidence
  -- | An existing named instance
  = NamedInstance (Qualified Ident)

  -- | Computed instances
  | WarnInstance SourceType -- ^ Warn type class with a user-defined warning message
  | IsSymbolInstance PSString -- ^ The IsSymbol type class for a given Symbol literal
  | ReflectableInstance Reflectable -- ^ The Reflectable type class for a reflectable kind
  | EmptyClassInstance        -- ^ For any solved type class with no members
  deriving (Show, Eq)

-- | Describes kinds that are reflectable to the term-level
data Reflectable
  = ReflectableInt Integer -- ^ For type-level numbers
  | ReflectableString PSString -- ^ For type-level strings
  | ReflectableBoolean Bool -- ^ For type-level booleans
  | ReflectableOrdering Ordering -- ^ For type-level orderings
  deriving (Show, Eq)

-- | Reflect a reflectable type into an expression
asExpression :: Reflectable -> Expr
asExpression = \case
  ReflectableInt n -> Literal NullSourceSpan $ NumericLiteral $ Left n
  ReflectableString s -> Literal NullSourceSpan $ StringLiteral s
  ReflectableBoolean b -> Literal NullSourceSpan $ BooleanLiteral b
  ReflectableOrdering o -> Constructor NullSourceSpan $ case o of
    LT -> C.C_LT
    EQ -> C.C_EQ
    GT -> C.C_GT

-- | Extract the identifier of a named instance
namedInstanceIdentifier :: Evidence -> Maybe (Qualified Ident)
namedInstanceIdentifier (NamedInstance i) = Just i
namedInstanceIdentifier _ = Nothing

-- | Description of a type class dictionary with instance evidence
type TypeClassDict = TypeClassDictionaryInScope Evidence

-- | The 'InstanceContext' tracks those constraints which can be satisfied.
type InstanceContext = M.Map QualifiedBy
                         (M.Map (Qualified (ProperName 'ClassName))
                           (M.Map (Qualified Ident) (NonEmpty NamedDict)))

findDicts :: InstanceContext -> Qualified (ProperName 'ClassName) -> QualifiedBy -> [TypeClassDict]
findDicts ctx cn = fmap (fmap NamedInstance) . foldMap NEL.toList . foldMap M.elems . (M.lookup cn <=< flip M.lookup ctx)

-- | A type substitution which makes an instance head match a list of types.
--
-- Note: we store many types per type variable name. For any name, all types
-- should unify if we are going to commit to an instance.
type Matching a = M.Map Text a

combineContexts :: InstanceContext -> InstanceContext -> InstanceContext
combineContexts = M.unionWith (M.unionWith (M.unionWith (<>)))

-- | Replace type class dictionary placeholders with inferred type class dictionaries
replaceTypeClassDictionaries
  :: 
   Bool
  -> Expr
  -> TypeCheckM (Expr, [(Ident, InstanceContext, SourceConstraint)])
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
    deferPass :: Expr -> StateT InstanceContext TypeCheckM (Expr, Any)
    deferPass = fmap (second fst) . runWriterT . f where
      f :: Expr -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext TypeCheckM) Expr
      (_, f, _) = everywhereOnValuesTopDownM return (go True) return

    -- This pass generalizes any remaining constraints
    generalizePass :: Expr -> StateT InstanceContext TypeCheckM (Expr, [(Ident, InstanceContext, SourceConstraint)])
    generalizePass = fmap (second snd) . runWriterT . f where
      f :: Expr -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext TypeCheckM) Expr
      (_, f, _) = everywhereOnValuesTopDownM return (go False) return

    go :: Bool -> Expr -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext TypeCheckM) Expr
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
  :: 
   SolverOptions
  -- ^ Solver options
  -> SourceConstraint
  -- ^ The constraint to solve
  -> InstanceContext
  -- ^ The contexts in which to solve the constraint
  -> [ErrorMessageHint]
  -- ^ Error message hints to apply to any instance errors
  -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext TypeCheckM) Expr
entails SolverOptions{..} constraint context hints =
  overConstraintArgsAll (lift . lift . traverse replaceAllTypeSynonyms) constraint >>= solve
  where
    forClassNameM :: Environment -> InstanceContext -> Qualified (ProperName 'ClassName) -> [SourceType] -> [SourceType] -> TypeCheckM [TypeClassDict]
    forClassNameM env ctx cn@C.Coercible kinds args =
      fromMaybe (forClassName env ctx cn kinds args) <$>
        solveCoercible env ctx kinds args
    forClassNameM env ctx cn kinds args =
      pure $ forClassName env ctx cn kinds args

    forClassName :: Environment -> InstanceContext -> Qualified (ProperName 'ClassName) -> [SourceType] -> [SourceType] -> [TypeClassDict]
    forClassName _ ctx cn@C.Warn _ [msg] =
      -- Prefer a warning dictionary in scope if there is one available.
      -- This allows us to defer a warning by propagating the constraint.
      findDicts ctx cn ByNullSourcePos ++ [TypeClassDictionaryInScope Nothing 0 (WarnInstance msg) [] C.Warn [] [] [msg] Nothing Nothing]
    forClassName _ _ C.IsSymbol _ args | Just dicts <- solveIsSymbol args = dicts
    forClassName _ _ C.SymbolCompare _ args | Just dicts <- solveSymbolCompare args = dicts
    forClassName _ _ C.SymbolAppend _ args | Just dicts <- solveSymbolAppend args = dicts
    forClassName _ _ C.SymbolCons _ args | Just dicts <- solveSymbolCons args = dicts
    forClassName _ _ C.IntAdd _ args | Just dicts <- solveIntAdd args = dicts
    forClassName _ ctx C.IntCompare _ args | Just dicts <- solveIntCompare ctx args = dicts
    forClassName _ _ C.IntMul _ args | Just dicts <- solveIntMul args = dicts
    forClassName _ _ C.IntToString _ args | Just dicts <- solveIntToString args = dicts
    forClassName _ _ C.Reflectable _ args | Just dicts <- solveReflectable args = dicts
    forClassName _ _ C.RowUnion kinds args | Just dicts <- solveUnion kinds args = dicts
    forClassName _ _ C.RowNub kinds args | Just dicts <- solveNub kinds args = dicts
    forClassName _ _ C.RowLacks kinds args | Just dicts <- solveLacks kinds args = dicts
    forClassName _ _ C.RowCons kinds args | Just dicts <- solveRowCons kinds args = dicts
    forClassName _ _ C.RowToList kinds args | Just dicts <- solveRowToList kinds args = dicts
    forClassName _ ctx cn@(Qualified (ByModuleName mn) _) _ tys = concatMap (findDicts ctx cn) (ordNub (ByNullSourcePos : ByModuleName mn : map ByModuleName (mapMaybe ctorModules tys)))
    forClassName _ _ _ _ _ = internalError "forClassName: expected qualified class name"

    ctorModules :: SourceType -> Maybe ModuleName
    ctorModules (TypeConstructor _ (Qualified (ByModuleName mn) _)) = Just mn
    ctorModules (TypeConstructor _ (Qualified (BySourcePos _) _)) = internalError "ctorModules: unqualified type name"
    ctorModules (TypeApp _ ty _) = ctorModules ty
    ctorModules (KindApp _ ty _) = ctorModules ty
    ctorModules (KindedType _ ty _) = ctorModules ty
    ctorModules _ = Nothing

    valUndefined :: Expr
    valUndefined = Var nullSourceSpan C.I_undefined

    solve :: SourceConstraint -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext TypeCheckM) Expr
    solve = go 0 hints
      where
        go :: Int -> [ErrorMessageHint] -> SourceConstraint -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext TypeCheckM) Expr
        go work _ (Constraint _ className' _ tys' _) | work > 1000 = throwError . errorMessage $ PossiblyInfiniteInstance className' tys'
        go work hints' con@(Constraint _ className' kinds' tys' conInfo) = WriterT . StateT . (withErrorMessageHint (ErrorSolvingConstraint con) .) . runStateT . runWriterT $ do
            -- We might have unified types by solving other constraints, so we need to
            -- apply the latest substitution.
            latestSubst <- lift . lift $ gets checkSubstitution
            let kinds'' = map (substituteType latestSubst) kinds'
                tys'' = map (substituteType latestSubst) tys'

            -- Get the inferred constraint context so far, and merge it with the global context
            inferred <- lift get
            -- We need information about functional dependencies, so we have to look up the class
            -- name in the environment:
            env <- lift . lift $ gets checkEnv
            let classesInScope = typeClasses env
            TypeClassData
              { typeClassArguments
              , typeClassDependencies
              , typeClassIsEmpty
              , typeClassCoveringSets
              , typeClassMembers 
              } <- case M.lookup className' classesInScope of
                Nothing -> throwError . errorMessage $ UnknownClass className'
                Just tcd -> pure tcd

            dicts <- lift . lift $ forClassNameM env (combineContexts context inferred) className' kinds'' tys''

            let (catMaybes -> ambiguous, instances) = partitionEithers $ do
                  chain :: NonEmpty TypeClassDict <-
                           NEL.groupBy ((==) `on` tcdChain) $
                           sortOn (tcdChain &&& tcdIndex)
                           dicts
                  -- process instances in a chain in index order
                  let found = for (tails1 chain) $ \(tcd :| tl) ->
                                -- Make sure the type unifies with the type in the type instance definition
                                case matches typeClassDependencies tcd tys'' of
                                  Apart        -> Right ()                   -- keep searching
                                  Match substs -> Left (Right (substs, tcd)) -- found a match
                                  Unknown ->
                                    if null (tcdChain tcd) || null tl
                                    then Right ()                                   -- need proof of apartness but this is either not in a chain or at the end
                                    else Left (Left (tcdToInstanceDescription tcd)) -- can't continue with this chain yet, need proof of apartness

                  lefts [found]
            solution <- lift . lift 
              $ unique kinds'' tys'' ambiguous instances 
              $ unknownsInAllCoveringSets (fst . (typeClassArguments !!)) typeClassMembers tys'' typeClassCoveringSets
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
                args <- solveSubgoals subst'' (ErrorSolvingConstraint con) (tcdDependencies tcd)

                initDict <- lift . lift $ mkDictionary (tcdValue tcd) args

                let match = foldr (\(className, index) dict -> subclassDictionaryValue dict className index)
                                  initDict
                                  (tcdPath tcd)

                return (if typeClassIsEmpty then Unused match else match)
              Unsolved unsolved -> do
                -- Generate a fresh name for the unsolved constraint's new dictionary
                ident <- freshIdent ("dict" <> runProperName (disqualify (constraintClass unsolved)))
                let qident = Qualified ByNullSourcePos ident
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
                return (TypeClassDictionary (srcConstraint className' kinds'' tys'' conInfo) context hints')
          where
            -- When checking functional dependencies, we need to use unification to make
            -- sure it is safe to use the selected instance. We will unify the solved type with
            -- the type in the instance head under the substitution inferred from its instantiation.
            -- As an example, when solving MonadState t0 (State Int), we choose the
            -- MonadState s (State s) instance, and we unify t0 with Int, since the functional
            -- dependency from MonadState dictates that t0 should unify with s\[s -> Int], which is
            -- Int. This is fine, but in some cases, the substitution does not remove all TypeVars
            -- from the type, so we end up with a unification error. So, any type arguments which
            -- appear in the instance head, but not in the substitution need to be replaced with
            -- fresh type variables. This function extends a substitution with fresh type variables
            -- as necessary, based on the types in the instance head. It also unifies kinds based on
            -- the substitution so kind information propagates correctly through the solver.
            withFreshTypes
              :: TypeClassDict
              -> Matching SourceType
              -> TypeCheckM (Matching SourceType)
            withFreshTypes TypeClassDictionaryInScope{..} initSubst = do
                subst <- foldM withFreshType initSubst $ filter (flip M.notMember initSubst . fst) tcdForAll
                for_ (M.toList initSubst) $ unifySubstKind subst
                pure subst
              where
                withFreshType subst (var, kind) = do
                  ty <- freshTypeWithKind $ replaceAllTypeVars (M.toList subst) kind
                  pure $ M.insert var ty subst

                unifySubstKind subst (var, ty) =
                  for_ (lookup var tcdForAll) $ \instKind -> do
                    tyKind <- elaborateKind ty
                    currentSubst <- gets checkSubstitution
                    unifyKinds'
                      (substituteType currentSubst . replaceAllTypeVars (M.toList subst) $ instKind)
                      (substituteType currentSubst tyKind)

            unique :: [SourceType] -> [SourceType] -> [Qualified (Either SourceType Ident)] -> [(a, TypeClassDict)] -> UnknownsHint -> TypeCheckM (EntailsResult a)
            unique kindArgs tyArgs ambiguous [] unks
              | solverDeferErrors = return Deferred
              -- We need a special case for nullary type classes, since we want
              -- to generalize over Partial constraints.
              | solverShouldGeneralize && ((null kindArgs && null tyArgs) || any canBeGeneralized kindArgs || any canBeGeneralized tyArgs) =
                  return (Unsolved (srcConstraint className' kindArgs tyArgs conInfo))
              | otherwise = throwError . errorMessage $ NoInstanceFound (srcConstraint className' kindArgs tyArgs conInfo) ambiguous unks
            unique _ _ _ [(a, dict)] _ = return $ Solved a dict
            unique _ tyArgs _ tcds _
              | pairwiseAny overlapping (map snd tcds) =
                  throwError . errorMessage $ OverlappingInstances className' tyArgs (tcds >>= (toList . tcdToInstanceDescription . snd))
              | otherwise = return $ uncurry Solved (minimumBy (compare `on` length . tcdPath . snd) tcds)

            tcdToInstanceDescription :: TypeClassDict -> Maybe (Qualified (Either SourceType Ident))
            tcdToInstanceDescription TypeClassDictionaryInScope{ tcdDescription, tcdValue } =
              let nii = namedInstanceIdentifier tcdValue
              in case tcdDescription of
                Just ty -> flip Qualified (Left ty) <$> fmap (byMaybeModuleName . getQual) nii
                Nothing -> fmap Right <$> nii

            canBeGeneralized :: Type a -> Bool
            canBeGeneralized TUnknown{} = True
            canBeGeneralized (KindedType _ t _) = canBeGeneralized t
            canBeGeneralized _ = False

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
            solveSubgoals :: Matching SourceType -> ErrorMessageHint -> Maybe [SourceConstraint] -> WriterT (Any, [(Ident, InstanceContext, SourceConstraint)]) (StateT InstanceContext TypeCheckM) (Maybe [Expr])
            solveSubgoals _ _ Nothing = return Nothing
            solveSubgoals subst hint (Just subgoals) =
              Just <$> traverse (rethrow (addHint hint) . go (work + 1) (hints' <> [hint]) . mapConstraintArgsAll (map (replaceAllTypeVars (M.toList subst)))) subgoals

            -- We need subgoal dictionaries to appear in the term somewhere
            -- If there aren't any then the dictionary is just undefined
            useEmptyDict :: Maybe [Expr] -> Expr
            useEmptyDict args = Unused (foldl (App . Abs (VarBinder nullSourceSpan UnusedIdent)) valUndefined (fold args))

            -- Make a dictionary from subgoal dictionaries by applying the correct function
            mkDictionary :: Evidence -> Maybe [Expr] -> TypeCheckM Expr
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
              return $ App (Constructor nullSourceSpan (coerceProperName . dictTypeName <$> C.IsSymbol)) (Literal nullSourceSpan (ObjectLiteral fields))
            mkDictionary (ReflectableInstance ref) _ =
              let fields = [ ("reflectType", Abs (VarBinder nullSourceSpan UnusedIdent) (asExpression ref)) ] in
              pure $ App (Constructor nullSourceSpan (coerceProperName . dictTypeName <$> C.Reflectable)) (Literal nullSourceSpan (ObjectLiteral fields))

            unknownsInAllCoveringSets :: (Int -> Text) -> [(Ident, SourceType, Maybe (S.Set (NEL.NonEmpty Int)))] -> [SourceType] -> S.Set (S.Set Int) -> UnknownsHint
            unknownsInAllCoveringSets indexToArgText tyClassMembers tyArgs coveringSets = do
              let unkIndices = findIndices containsUnknowns tyArgs
              if all (\s -> any (`S.member` s) unkIndices) coveringSets then 
                fromMaybe Unknowns unknownsRequiringVtas
              else 
                NoUnknowns
              where
                unknownsRequiringVtas = do
                  tyClassModuleName <- getQual className'
                  let
                    tyClassMemberVta :: M.Map (Qualified Ident) [[Text]]
                    tyClassMemberVta = M.fromList $ mapMaybe qualifyAndFilter tyClassMembers
                      where
                        -- Only keep type class members that need VTAs to resolve their type class instances
                        qualifyAndFilter (ident, _, mbVtaRequiredArgs) = mbVtaRequiredArgs <&> \vtaRequiredArgs ->
                          (Qualified (ByModuleName tyClassModuleName) ident, map (map indexToArgText . NEL.toList) $ S.toList vtaRequiredArgs)

                    tyClassMembersInExpr :: Expr -> [(Qualified Ident, [[Text]])]
                    tyClassMembersInExpr = getVars
                      where
                        (_, getVars, _, _, _) = everythingOnValues (++) ignore getVarIdents ignore ignore ignore
                        ignore = const []
                        getVarIdents = \case
                          Var _ ident | Just vtas <- M.lookup ident tyClassMemberVta -> 
                            [(ident, vtas)]
                          _ -> 
                            []

                    getECTExpr = \case
                      ErrorCheckingType expr _ -> Just expr
                      _ -> Nothing
                      
                  tyClassMembers' <- headMay $ mapMaybe (fmap tyClassMembersInExpr . getECTExpr) hints
                  membersWithVtas <- NEL.nonEmpty tyClassMembers'
                  pure $ UnknownsWithVtaRequiringArgs membersWithVtas

        -- Turn a DictionaryValue into a Expr
        subclassDictionaryValue :: Expr -> Qualified (ProperName 'ClassName) -> Integer -> Expr
        subclassDictionaryValue dict className index =
          App (Accessor (mkString (superclassName className index)) dict) valUndefined

    solveCoercible :: Environment -> InstanceContext -> [SourceType] -> [SourceType] -> TypeCheckM (Maybe [TypeClassDict])
    solveCoercible env ctx kinds [a, b] = do
      let coercibleDictsInScope = findDicts ctx C.Coercible ByNullSourcePos
          givens = flip mapMaybe coercibleDictsInScope $ \case
            dict | [a', b'] <- tcdInstanceTypes dict -> Just (a', b')
                 | otherwise -> Nothing
      GivenSolverState{ inertGivens } <- execStateT (solveGivens env) $
        initialGivenSolverState givens
      (WantedSolverState{ inertWanteds }, hints') <- runWriterT . execStateT (solveWanteds env) $
        initialWantedSolverState inertGivens a b
      -- Solving fails when there's irreducible wanteds left.
      --
      -- We report the first residual constraint instead of the initial wanted,
      -- unless we just swapped its arguments.
      --
      -- We may have collected hints for the solving failure along the way, in
      -- which case we decorate the error with the first one.
      maybe id addHint (listToMaybe hints') `rethrow` case inertWanteds of
        [] -> pure $ Just [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.Coercible [] kinds [a, b] Nothing Nothing]
        (k, a', b') : _ | a' == b && b' == a -> throwError $ insoluble k b' a'
        (k, a', b') : _ -> throwError $ insoluble k a' b'
    solveCoercible _ _ _ _ = pure Nothing

    solveIsSymbol :: [SourceType] -> Maybe [TypeClassDict]
    solveIsSymbol [TypeLevelString ann sym] = Just [TypeClassDictionaryInScope Nothing 0 (IsSymbolInstance sym) [] C.IsSymbol [] [] [TypeLevelString ann sym] Nothing Nothing]
    solveIsSymbol _ = Nothing

    solveSymbolCompare :: [SourceType] -> Maybe [TypeClassDict]
    solveSymbolCompare [arg0@(TypeLevelString _ lhs), arg1@(TypeLevelString _ rhs), _] =
      let ordering = case compare lhs rhs of
                  LT -> C.LT
                  EQ -> C.EQ
                  GT -> C.GT
          args' = [arg0, arg1, srcTypeConstructor ordering]
      in Just [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.SymbolCompare [] [] args' Nothing Nothing]
    solveSymbolCompare _ = Nothing

    solveSymbolAppend :: [SourceType] -> Maybe [TypeClassDict]
    solveSymbolAppend [arg0, arg1, arg2] = do
      (arg0', arg1', arg2') <- appendSymbols arg0 arg1 arg2
      let args' = [arg0', arg1', arg2']
      pure [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.SymbolAppend [] [] args' Nothing Nothing]
    solveSymbolAppend _ = Nothing

    -- Append type level symbols, or, run backwards, strip a prefix or suffix
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
      pure [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.SymbolCons [] [] args' Nothing Nothing]
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

    solveIntToString :: [SourceType] -> Maybe [TypeClassDict]
    solveIntToString [arg0, _] = do
      (arg0', arg1') <- printIntToString arg0
      let args' = [arg0', arg1']
      pure [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.IntToString [] [] args' Nothing Nothing]
    solveIntToString _ = Nothing

    printIntToString :: SourceType -> Maybe (SourceType, SourceType)
    printIntToString arg0@(TypeLevelInt _ i) = do
      pure (arg0, srcTypeLevelString $ mkString $ T.pack $ show i)
    printIntToString _ = Nothing

    solveReflectable :: [SourceType] -> Maybe [TypeClassDict]
    solveReflectable [typeLevel, _] = do
      (ref, typ) <- case typeLevel of
        TypeLevelInt _ i -> pure (ReflectableInt i, tyInt)
        TypeLevelString _ s -> pure (ReflectableString s, tyString)
        TypeConstructor _ n
          | n == C.True -> pure (ReflectableBoolean True, tyBoolean)
          | n == C.False -> pure (ReflectableBoolean False, tyBoolean)
          | n == C.LT -> pure (ReflectableOrdering LT, srcTypeConstructor C.Ordering)
          | n == C.EQ -> pure (ReflectableOrdering EQ, srcTypeConstructor C.Ordering)
          | n == C.GT -> pure (ReflectableOrdering GT, srcTypeConstructor C.Ordering)
        _ -> Nothing
      pure [TypeClassDictionaryInScope Nothing 0 (ReflectableInstance ref) [] C.Reflectable [] [] [typeLevel, typ] Nothing Nothing]
    solveReflectable _ = Nothing

    solveIntAdd :: [SourceType] -> Maybe [TypeClassDict]
    solveIntAdd [arg0, arg1, arg2] = do
      (arg0', arg1', arg2') <- addInts arg0 arg1 arg2
      let args' = [arg0', arg1', arg2']
      pure [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.IntAdd [] [] args' Nothing Nothing]
    solveIntAdd _ = Nothing

    addInts :: SourceType -> SourceType -> SourceType -> Maybe (SourceType, SourceType, SourceType)
    -- l r -> o, l + r = o
    addInts arg0@(TypeLevelInt _ l) arg1@(TypeLevelInt _ r) _ = pure (arg0, arg1, srcTypeLevelInt (l + r))
    -- l o -> r, o - l = r
    addInts arg0@(TypeLevelInt _ l) _ arg2@(TypeLevelInt _ o) = pure (arg0, srcTypeLevelInt (o - l), arg2)
    -- r o -> l, o - r = l
    addInts _ arg1@(TypeLevelInt _ r) arg2@(TypeLevelInt _ o) = pure (srcTypeLevelInt (o - r), arg1, arg2)
    addInts _ _ _                                             = Nothing

    solveIntCompare :: InstanceContext -> [SourceType] -> Maybe [TypeClassDict]
    solveIntCompare _ [arg0@(TypeLevelInt _ a), arg1@(TypeLevelInt _ b), _] =
      let ordering = case compare a b of
            EQ -> C.EQ
            LT -> C.LT
            GT -> C.GT
          args' = [arg0, arg1, srcTypeConstructor ordering]
      in pure [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.IntCompare [] [] args' Nothing Nothing]
    solveIntCompare ctx args@[a, b, _] = do
      let compareDictsInScope = findDicts ctx C.IntCompare ByNullSourcePos
          givens = flip mapMaybe compareDictsInScope $ \case
            dict | [a', b', c'] <- tcdInstanceTypes dict -> mkRelation a' b' c'
                 | otherwise -> Nothing
          facts = mkFacts (args : (tcdInstanceTypes <$> compareDictsInScope))
      c' <- solveRelation (givens <> facts) a b
      pure [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.IntCompare [] [] [a, b, srcTypeConstructor c'] Nothing Nothing]
    solveIntCompare _ _ = Nothing

    solveIntMul :: [SourceType] -> Maybe [TypeClassDict]
    solveIntMul [arg0@(TypeLevelInt _ l), arg1@(TypeLevelInt _ r), _] =
      let args' = [arg0, arg1, srcTypeLevelInt (l * r)]
      in pure [TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.IntMul [] [] args' Nothing Nothing]
    solveIntMul _ = Nothing

    solveUnion :: [SourceType] -> [SourceType] -> Maybe [TypeClassDict]
    solveUnion kinds [l, r, u] = do
      (lOut, rOut, uOut, cst, vars) <- unionRows kinds l r u
      pure [ TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.RowUnion vars kinds [lOut, rOut, uOut] cst Nothing ]
    solveUnion _ _ = Nothing

    -- Left biased union of two row types

    unionRows :: [SourceType] -> SourceType -> SourceType -> SourceType -> Maybe (SourceType, SourceType, SourceType, Maybe [SourceConstraint], [(Text, SourceType)])
    unionRows kinds l r u =
        guard canMakeProgress $> (lOut, rOut, uOut, cons, vars)
      where
        (fixed, rest) = rowToList l

        rowVar = srcTypeVar "r"

        (canMakeProgress, lOut, rOut, uOut, cons, vars) =
          case rest of
            -- If the left hand side is a closed row, then we can merge
            -- its labels into the right hand side.
            REmptyKinded _ _ -> (True, l, r, rowFromList (fixed, r), Nothing, [])
            -- If the right hand side and output are closed rows, then we can
            -- compute the left hand side by subtracting the right hand side
            -- from the output.
            _ | (right, rightu@(REmptyKinded _ _)) <- rowToList r
              , (output, restu@(REmptyKinded _ _)) <- rowToList u ->
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
              in ( null leftover
                 , rowFromList (outL, restu)
                 , rowFromList (outR, rightu)
                 , u
                 , Nothing
                 , []
                 )
            -- If the left hand side is not definitely closed, then the only way we
            -- can safely make progress is to move any known labels from the left
            -- input into the output, and add a constraint for any remaining labels.
            -- Otherwise, the left hand tail might contain the same labels as on
            -- the right hand side, and we can't be certain we won't reorder the
            -- types for such labels.
            _ -> ( not (null fixed)
                 , l, r
                 , rowFromList (fixed, rowVar)
                 , Just [ srcConstraint C.RowUnion kinds [rest, r, rowVar] Nothing ]
                 , [("r", kindRow (head kinds))]
                 )

    solveRowCons :: [SourceType] -> [SourceType] -> Maybe [TypeClassDict]
    solveRowCons kinds [TypeLevelString ann sym, ty, r, _] =
      Just [ TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.RowCons [] kinds [TypeLevelString ann sym, ty, r, srcRCons (Label sym) ty r] Nothing Nothing ]
    solveRowCons _ _ = Nothing

    solveRowToList :: [SourceType] -> [SourceType] -> Maybe [TypeClassDict]
    solveRowToList [kind] [r, _] = do
      entries <- rowToRowList kind r
      pure [ TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.RowToList [] [kind] [r, entries] Nothing Nothing ]
    solveRowToList _ _ = Nothing

    -- Convert a closed row to a sorted list of entries
    rowToRowList :: SourceType -> SourceType -> Maybe SourceType
    rowToRowList kind r =
        guard (isREmpty rest) $>
        foldr rowListCons (srcKindApp (srcTypeConstructor C.RowListNil) kind) fixed
      where
        (fixed, rest) = rowToSortedList r
        rowListCons (RowListItem _ lbl ty) tl =
          foldl srcTypeApp (srcKindApp (srcTypeConstructor C.RowListCons) kind)
            [ srcTypeLevelString (runLabel lbl)
            , ty
            , tl ]

    solveNub :: [SourceType] -> [SourceType] -> Maybe [TypeClassDict]
    solveNub kinds [r, _] = do
      r' <- nubRows r
      pure [ TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.RowNub [] kinds [r, r'] Nothing Nothing ]
    solveNub _ _ = Nothing

    nubRows :: SourceType -> Maybe SourceType
    nubRows r =
        guard (isREmpty rest) $>
        rowFromList (nubBy ((==) `on` rowListLabel) fixed, rest)
      where
        (fixed, rest) = rowToSortedList r

    solveLacks :: [SourceType] -> [SourceType] -> Maybe [TypeClassDict]
    solveLacks kinds tys@[_, REmptyKinded _ _] =
      pure [ TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.RowLacks [] kinds tys Nothing Nothing ]
    solveLacks kinds [TypeLevelString ann sym, r] = do
      (r', cst) <- rowLacks kinds sym r
      pure [ TypeClassDictionaryInScope Nothing 0 EmptyClassInstance [] C.RowLacks [] kinds [TypeLevelString ann sym, r'] cst Nothing ]
    solveLacks _ _ = Nothing

    rowLacks :: [SourceType] -> PSString -> SourceType -> Maybe (SourceType, Maybe [SourceConstraint])
    rowLacks kinds sym r =
        guard (lacksSym && canMakeProgress) $> (r, cst)
      where
        (fixed, rest) = rowToList r

        lacksSym =
          sym `notElem` (runLabel . rowListLabel <$> fixed)

        (canMakeProgress, cst) = case rest of
            REmptyKinded _ _ -> (True, Nothing)
            _ -> (not (null fixed), Just [ srcConstraint C.RowLacks kinds [srcTypeLevelString sym, rest] Nothing ])

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
    -- Find the closure of a set of functional dependencies.
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
    typeHeadsAreEqual (Skolem _ _ _ s1 _)      (Skolem _ _ _ s2 _)    | s1 == s2   = (Match (), M.empty)
    typeHeadsAreEqual t                      (TypeVar _ v)                     = (Match (), M.singleton v [t])
    typeHeadsAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = (Match (), M.empty)
    typeHeadsAreEqual (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = (Match (), M.empty)
    typeHeadsAreEqual (TypeLevelInt _ n1)    (TypeLevelInt _ n2)    | n1 == n2 = (Match (), M.empty)
    typeHeadsAreEqual (TypeApp _ h1 t1)      (TypeApp _ h2 t2)                 =
      both (typeHeadsAreEqual h1 h2) (typeHeadsAreEqual t1 t2)
    typeHeadsAreEqual (KindApp _ h1 t1)      (KindApp _ h2 t2)                 =
      both (typeHeadsAreEqual h1 h2) (typeHeadsAreEqual t1 t2)
    typeHeadsAreEqual (REmpty _) (REmpty _) = (Match (), M.empty)
    typeHeadsAreEqual r1@RCons{} r2@RCons{} =
        foldr both (uncurry go rest) common
      where
        (common, rest) = alignRowsWith (const typeHeadsAreEqual) r1 r2

        go :: ([RowListItem a], Type a) -> ([RowListItem a], Type a) -> (Matched (), Matching [Type a])
        go (l,  KindedType _ t1 _) (r,  t2)                            = go (l, t1) (r, t2)
        go (l,  t1)                (r,  KindedType _ t2 _)             = go (l, t1) (r, t2)
        go (l,  KindApp _ t1 k1)   (r,  KindApp _ t2 k2) | eqType k1 k2 = go (l, t1) (r, t2)
        go ([], REmpty _)          ([], REmpty _)                      = (Match (), M.empty)
        go ([], TUnknown _ u1)     ([], TUnknown _ u2)      | u1 == u2 = (Match (), M.empty)
        go ([], TypeVar _ v1)      ([], TypeVar _ v2)       | v1 == v2 = (Match (), M.empty)
        go ([], Skolem _ _ _ sk1 _)  ([], Skolem _ _ _ sk2 _) | sk1 == sk2 = (Match (), M.empty)
        go ([], TUnknown _ _)      _                                   = (Unknown, M.empty)
        go (sd, r)                 ([], TypeVar _ v)                   = (Match (), M.singleton v [rowFromList (sd, r)])
        go _ _                                                         = (Apart, M.empty)
    typeHeadsAreEqual (TUnknown _ _) _ = (Unknown, M.empty)
    typeHeadsAreEqual Skolem{} _       = (Unknown, M.empty)
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
      typesAreEqual (TUnknown _ u1)        t2                     = if t2 `containsUnknown` u1 then Apart else Unknown
      typesAreEqual t1                     (TUnknown _ u2)        = if t1 `containsUnknown` u2 then Apart else Unknown
      typesAreEqual (Skolem _ _ _ s1 _)    (Skolem _ _ _ s2 _)    | s1 == s2 = Match ()
      typesAreEqual (Skolem _ _ _ s1 _)    t2                     = if t2 `containsSkolem` s1 then Apart else Unknown
      typesAreEqual t1                     (Skolem _ _ _ s2 _)    = if t1 `containsSkolem` s2 then Apart else Unknown
      typesAreEqual (TypeVar _ v1)         (TypeVar _ v2)         | v1 == v2 = Match ()
      typesAreEqual (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = Match ()
      typesAreEqual (TypeLevelInt _ n1)    (TypeLevelInt _ n2)    | n1 == n2 = Match ()
      typesAreEqual (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = Match ()
      typesAreEqual (TypeApp _ h1 t1)      (TypeApp _ h2 t2)      = typesAreEqual h1 h2 <> typesAreEqual t1 t2
      typesAreEqual (KindApp _ h1 t1)      (KindApp _ h2 t2)      = typesAreEqual h1 h2 <> typesAreEqual t1 t2
      typesAreEqual (REmpty _)             (REmpty _)             = Match ()
      typesAreEqual r1                     r2                     | isRCons r1 || isRCons r2 =
          let (common, rest) = alignRowsWith (const typesAreEqual) r1 r2
          in fold common <> uncurry go rest
        where
          go :: ([RowListItem a], Type a) -> ([RowListItem a], Type a) -> Matched ()
          go (l, KindedType _ t1 _) (r, t2)                           = go (l, t1) (r, t2)
          go (l, t1)                (r, KindedType _ t2 _)            = go (l, t1) (r, t2)
          go ([], KindApp _ t1 k1)  ([], KindApp _ t2 k2)             = typesAreEqual t1 t2 <> typesAreEqual k1 k2
          go ([], TUnknown _ u1)    ([], TUnknown _ u2)    | u1 == u2 = Match ()
          go ([], TUnknown _ _)     ([], _)                           = Unknown
          go ([], _)                ([], TUnknown _ _)                = Unknown
          go ([], Skolem _ _ _ s1 _)  ([], Skolem _ _ _ s2 _)  | s1 == s2 = Match ()
          go ([], Skolem _ _ _ _ _)   _                               = Unknown
          go _                      ([], Skolem _ _ _ _ _)            = Unknown
          go ([], REmpty _)         ([], REmpty _)                    = Match ()
          go ([], TypeVar _ v1)     ([], TypeVar _ v2)     | v1 == v2 = Match ()
          go _  _                                                     = Apart
      typesAreEqual _               _                                 = Apart

      isRCons :: Type a -> Bool
      isRCons RCons{}    = True
      isRCons _          = False

      containsSkolem :: Type a -> Int -> Bool
      containsSkolem t s = everythingOnTypes (||) (\case Skolem _ _ _ s' _ -> s == s'; _ -> False) t

      containsUnknown :: Type a -> Int -> Bool
      containsUnknown t u = everythingOnTypes (||) (\case TUnknown _ u' -> u == u'; _ -> False) t

-- | Add a dictionary for the constraint to the scope, and dictionaries
-- for all implied superclass instances.
newDictionaries
  :: MonadState CheckState m
  => [(Qualified (ProperName 'ClassName), Integer)]
  -> Qualified Ident
  -> SourceConstraint
  -> m [NamedDict]
newDictionaries path name (Constraint _ className instanceKinds instanceTy _) = do
    tcs <- gets (typeClasses . checkEnv)
    let TypeClassData{..} = fromMaybe (internalError "newDictionaries: type class lookup failed") $ M.lookup className tcs
    supDicts <- join <$> zipWithM (\(Constraint ann supName supKinds supArgs _) index ->
                                      let sub = zip (map fst typeClassArguments) instanceTy in
                                      newDictionaries ((supName, index) : path)
                                                      name
                                                      (Constraint ann supName
                                                        (replaceAllTypeVars sub <$> supKinds)
                                                        (replaceAllTypeVars sub <$> supArgs)
                                                        Nothing)
                                  ) typeClassSuperclasses [0..]
    return (TypeClassDictionaryInScope Nothing 0 name path className [] instanceKinds instanceTy Nothing Nothing : supDicts)

mkContext :: [NamedDict] -> InstanceContext
mkContext = foldr combineContexts M.empty . map fromDict where
  fromDict d = M.singleton ByNullSourcePos (M.singleton (tcdClassName d) (M.singleton (tcdValue d) (pure d)))

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

-- | Return all nonempty tails of a nonempty list. For example:
--
-- tails1 (fromList [1]) == fromList [fromList [1]]
-- tails1 (fromList [1,2]) == fromList [fromList [1,2], fromList [2]]
-- tails1 (fromList [1,2,3]) == fromList [fromList [1,2,3], fromList [2,3], fromList [3]]
tails1 :: NonEmpty a -> NonEmpty (NonEmpty a)
tails1 =
  -- NEL.fromList is an unsafe function, but this usage should be safe, since:
  -- - `tails xs = [xs, tail xs, tail (tail xs), ..., []]`
  -- - If `xs` is nonempty, it follows that `tails xs` contains at least one nonempty
  --   list, since `head (tails xs) = xs`.
  -- - The only empty element of `tails xs` is the last one (by the definition of `tails`)
  -- - Therefore, if we take all but the last element of `tails xs` i.e.
  --   `init (tails xs)`, we have a nonempty list of nonempty lists
  NEL.fromList . map NEL.fromList . init . tails . NEL.toList
