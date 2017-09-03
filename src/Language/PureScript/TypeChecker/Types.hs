{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module implements the type checker
--
module Language.PureScript.TypeChecker.Types
  ( BindingGroupType(..)
  , typesOf
  ) where

{-
  The following functions represent the corresponding type checking judgements:

    infer
      Synthesize a type for a value

    check
      Check a value has a given type

    checkProperties
      Check an object with a given type contains specified properties

    checkFunctionApplication
      Check a function of a given type returns a value of another type when applied to its arguments
-}

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow (first, second, (***))
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.List (transpose, (\\), partition, delete)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Traversable (for)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Traversals
import Language.PureScript.TypeChecker.Entailment
import Language.PureScript.TypeChecker.Kinds
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Skolems
import Language.PureScript.TypeChecker.Subsumption
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.TypeChecker.TypeSearch
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.Types
import Language.PureScript.Label (Label(..))
import Language.PureScript.PSString (PSString)

import Debug.Trace

data BindingGroupType
  = RecursiveBindingGroup
  | NonRecursiveBindingGroup
  deriving (Show, Eq, Ord)

-- | Infer the types of multiple mutually-recursive values, and return elaborated values including
-- type class dictionaries and type annotations.
typesOf
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => BindingGroupType
  -> ModuleName
  -> [((SourceAnn, Ident), Expr)]
  -> m [((SourceAnn, Ident), (Expr, Type))]
typesOf bindingGroupType moduleName vals = withFreshSubstitution $ do
    (tys, wInfer) <- capturingSubstitution tidyUp $ do
      (SplitBindingGroup untyped typed dict, w) <- withoutWarnings $ typeDictionaryForBindingGroup (Just moduleName) vals
      ds1 <- parU typed $ \e -> withoutWarnings $ checkTypedBindingGroupElement moduleName e dict
      ds2 <- forM untyped $ \e -> withoutWarnings $ typeForBindingGroupElement e dict
      return (map (False, ) ds1 ++ map (True, ) ds2, w)

    inferred <- forM tys $ \(shouldGeneralize, ((sai@((ss, _), ident), (val, ty)), _)) -> do
      -- Replace type class dictionary placeholders with actual dictionaries
      (val', unsolved) <- replaceTypeClassDictionaries shouldGeneralize val
      -- Generalize and constrain the type
      currentSubst <- gets checkSubstitution
      let ty' = substituteType currentSubst ty
          unsolvedTypeVars = ordNub $ unknownsInType ty'
          generalized = generalize unsolved ty'

      when shouldGeneralize $ do
        -- Show the inferred type in a warning
        tell
          . errorMessage' ss
          $ MissingTypeDeclaration ident generalized
        -- For non-recursive binding groups, can generalize over constraints.
        -- For recursive binding groups, we throw an error here for now.
        when (bindingGroupType == RecursiveBindingGroup && not (null unsolved))
          . throwError
          . errorMessage' ss
          $ CannotGeneralizeRecursiveFunction ident generalized
        -- Make sure any unsolved type constraints only use type variables which appear
        -- unknown in the inferred type.
        forM_ unsolved $ \(_, _, con) -> do
          -- We need information about functional dependencies, since we allow
          -- ambiguous types to be inferred if they can be solved by some functional
          -- dependency.
          let findClass = fromMaybe (internalError "entails: type class not found in environment") . M.lookup (constraintClass con)
          TypeClassData{ typeClassDependencies } <- gets (findClass . typeClasses . checkEnv)
          let solved = foldMap (S.fromList . fdDetermined) typeClassDependencies
          let constraintTypeVars = ordNub . foldMap (unknownsInType . fst) . filter ((`notElem` solved) . snd) $ zip (constraintArgs con) [0..]
          when (any (`notElem` unsolvedTypeVars) constraintTypeVars) .
            throwError
              . onErrorMessages (replaceTypes currentSubst)
              . errorMessage' ss
              $ AmbiguousTypeVariables generalized con

      -- Check skolem variables did not escape their scope
      skolemEscapeCheck val'
      let sa@(ss', _) = exprSourceAnn val'
      return ((sai, (foldr (Abs sa . VarBinder ss' . (\(x, _, _) -> x)) val' unsolved, generalized)), unsolved)

    -- Show warnings here, since types in wildcards might have been solved during
    -- instance resolution (by functional dependencies).
    finalState <- get
    let replaceTypes' = replaceTypes (checkSubstitution finalState)
        runTypeSearch' gen = runTypeSearch (guard gen $> foldMap snd inferred) finalState
        raisePreviousWarnings gen = (escalateWarningWhen isHoleError . tell . onErrorMessages (runTypeSearch' gen . replaceTypes'))

    raisePreviousWarnings False wInfer
    forM_ tys $ \(shouldGeneralize, ((_, (_, _)), w)) ->
      raisePreviousWarnings shouldGeneralize w

    return (map fst inferred)
  where
    replaceTypes
      :: Substitution
      -> ErrorMessage
      -> ErrorMessage
    replaceTypes subst = onTypesInErrorMessage (substituteType subst)

    -- | Run type search to complete any typed hole error messages
    runTypeSearch
      :: Maybe [(Ident, InstanceContext, Constraint)]
      -- ^ Any unsolved constraints which we need to continue to satisfy
      -> CheckState
      -- ^ The final type checker state
      -> ErrorMessage
      -> ErrorMessage
    runTypeSearch cons st = \case
      ErrorMessage hints (HoleInferredType x ty y (TSBefore env)) ->
        let subst = checkSubstitution st
            searchResult = onTypeSearchTypes
              (substituteType subst)
              (uncurry TSAfter (typeSearch cons env st (substituteType subst ty)))
        in ErrorMessage hints (HoleInferredType x ty y searchResult)
      other -> other

    -- | Generalize type vars using forall and add inferred constraints
    generalize unsolved = varIfUnknown . constrain unsolved

    -- | Add any unsolved constraints
    constrain cs ty = foldr ConstrainedType ty (map (\(_, _, x) -> x) cs)

    -- Apply the substitution that was returned from runUnify to both types and (type-annotated) values
    tidyUp ts sub = first (map (second (first (second (overTypes (substituteType sub) *** substituteType sub))))) ts

    isHoleError :: ErrorMessage -> Bool
    isHoleError (ErrorMessage _ HoleInferredType{}) = True
    isHoleError _ = False

-- | A binding group contains multiple value definitions, some of which are typed
-- and some which are not.
--
-- This structure breaks down a binding group into typed and untyped parts.
data SplitBindingGroup = SplitBindingGroup
  { _splitBindingGroupUntyped :: [((SourceAnn, Ident), (Expr, Type))]
  -- ^ The untyped expressions
  , _splitBindingGroupTyped :: [((SourceAnn, Ident), (Expr, Type, Bool))]
  -- ^ The typed expressions, along with their type annotations
  , _splitBindingGroupNames :: M.Map (Qualified Ident) (Type, NameKind, NameVisibility)
  -- ^ A map containing all expressions and their assigned types (which might be
  -- fresh unification variables). These will be added to the 'Environment' after
  -- the binding group is checked, so the value type of the 'Map' is chosen to be
  -- compatible with the type of 'bindNames'.
  }

-- | This function breaks a binding group down into two sets of declarations:
-- those which contain type annotations, and those which don't.
-- This function also generates fresh unification variables for the types of
-- declarations without type annotations, returned in the 'UntypedData' structure.
typeDictionaryForBindingGroup
  :: (MonadState CheckState m, MonadWriter MultipleErrors m)
  => Maybe ModuleName
  -> [((SourceAnn, Ident), Expr)]
  -> m SplitBindingGroup
typeDictionaryForBindingGroup moduleName vals = do
    -- Filter the typed and untyped declarations and make a map of names to typed declarations.
    -- Replace type wildcards here so that the resulting dictionary of types contains the
    -- fully expanded types.
    let (untyped, typed) = partitionEithers (map splitTypeAnnotation vals)
    (typedDict, typed') <- fmap unzip . for typed $ \(sai, (expr, ty, checkType)) -> do
      ty' <- replaceTypeWildcards ty
      return ((sai, ty'), (sai, (expr, ty', checkType)))
    -- Create fresh unification variables for the types of untyped declarations
    (untypedDict, untyped') <- fmap unzip . for untyped $ \(sai, expr) -> do
      ty <- freshType
      return ((sai, ty), (sai, (expr, ty)))
    -- Create the dictionary of all name/type pairs, which will be added to the
    -- environment during type checking
    let dict = M.fromList [ (Qualified moduleName ident, (ty, Private, Undefined))
                          | ((_, ident), ty) <- typedDict <> untypedDict
                          ]
    return (SplitBindingGroup untyped' typed' dict)
  where
    -- | Check if a value contains a type annotation, and if so, separate it
    -- from the value itself.

    -- TODO(Christoph): While every expression has a source span associated with
    -- it, we might want to show the span for typed expression in the errors,
    -- for example we don't want to just highlight x as the error in (x :: String)
    splitTypeAnnotation :: (a, Expr) -> Either (a, Expr) (a, (Expr, Type, Bool))
    splitTypeAnnotation (a, TypedValue _ checkType value ty) = Right (a, (value, ty, checkType))
    splitTypeAnnotation (a, value) = Left (a, value)

-- | Check the type annotation of a typed value in a binding group.
checkTypedBindingGroupElement
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> ((SourceAnn, Ident), (Expr, Type, Bool))
  -- ^ The identifier we are trying to define, along with the expression and its type annotation
  -> M.Map (Qualified Ident) (Type, NameKind, NameVisibility)
  -- ^ Names brought into scope in this binding group
  -> m ((SourceAnn, Ident), (Expr, Type))
checkTypedBindingGroupElement mn (ident, (val, ty, checkType)) dict = do
  -- Kind check
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  -- We replace type synonyms _after_ kind-checking, since we don't want type
  -- synonym expansion to bring type variables into scope. See #2542.
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  -- Check the type with the new names in scope
  val' <- if checkType
            then withScopedTypeVars mn args $ bindNames dict $ check val ty'
            else return (TypedValue (exprSourceAnn val) False val ty')
  return (ident, (val', ty'))

-- | Infer a type for a value in a binding group which lacks an annotation.
typeForBindingGroupElement
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ((SourceAnn, Ident), (Expr, Type))
  -- ^ The identifier we are trying to define, along with the expression and its assigned type
  -- (at this point, this should be a unification variable)
  -> M.Map (Qualified Ident) (Type, NameKind, NameVisibility)
  -- ^ Names brought into scope in this binding group
  -> m ((SourceAnn, Ident), (Expr, Type))
typeForBindingGroupElement (ident, (val, ty)) dict = do
  -- Infer the type with the new names in scope
  TypedValue sa _ val' ty' <- bindNames dict $ infer val
  -- Unify the type with the unification variable we chose for this definition
  unifyTypes ty ty'
  return (ident, (TypedValue sa True val' ty', ty'))

-- | Check the kind of a type, failing if it is not of kind *.
checkTypeKind
  :: MonadError MultipleErrors m
  => Type
  -> Kind
  -> m ()
checkTypeKind ty kind = guardWith (errorMessage (ExpectedType ty kind)) $ kind == kindType

-- | Remove any ForAlls and ConstrainedType constructors in a type by introducing new unknowns
-- or TypeClassDictionary values.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
instantiatePolyTypeWithUnknowns
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => Expr
  -> Type
  -> m (Expr, Type)
instantiatePolyTypeWithUnknowns val (ForAll ident ty _) = do
  ty' <- replaceVarWithUnknown ident ty
  instantiatePolyTypeWithUnknowns val ty'
instantiatePolyTypeWithUnknowns val (ConstrainedType con ty) = do
   dicts <- getTypeClassDictionaries
   hints <- getHints
   let sa = exprSourceAnn val
   instantiatePolyTypeWithUnknowns (App sa val (TypeClassDictionary sa con dicts hints)) ty
instantiatePolyTypeWithUnknowns val ty = return (val, ty)

-- | Infer a type for a value, rethrowing any error to provide a more useful error message
infer
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> m Expr
infer val = withErrorMessageHint (ErrorInferringType val) $ infer' val

-- | Infer a type for a value
infer'
  :: forall m
   . (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> m Expr
infer' v@(Literal sa (NumericLiteral (Left _))) = return $ TypedValue sa True v tyInt
infer' v@(Literal sa (NumericLiteral (Right _))) = return $ TypedValue sa True v tyNumber
infer' v@(Literal sa (StringLiteral _)) = return $ TypedValue sa True v tyString
infer' v@(Literal sa (CharLiteral _)) = return $ TypedValue sa True v tyChar
infer' v@(Literal sa (BooleanLiteral _)) = return $ TypedValue sa True v tyBoolean
infer' (Literal sa (ArrayLiteral vals)) = do
  ts <- traverse infer vals
  els <- freshType
  ts' <- forM ts $ \(TypedValue sa' ch val t) -> do
    (val', t') <- instantiatePolyTypeWithUnknowns val t
    unifyTypes els t'
    return (TypedValue sa' ch val' t')
  return $ TypedValue sa True (Literal sa (ArrayLiteral ts')) (TypeApp tyArray els)
infer' (Literal sa (ObjectLiteral ps)) = do
  ensureNoDuplicateProperties ps
  -- We make a special case for Vars in record labels, since these are the
  -- only types of expressions for which 'infer' can return a polymorphic type.
  -- They need to be instantiated here.
  let shouldInstantiate :: Expr -> Bool
      shouldInstantiate Var{} = True
      shouldInstantiate _ = False

      inferProperty :: (PSString, Expr) -> m (PSString, (Expr, Type))
      inferProperty (name, val) = do
        TypedValue _ _ val' ty <- infer val
        valAndType <- if shouldInstantiate val
                        then instantiatePolyTypeWithUnknowns val' ty
                        else pure (val', ty)
        pure (name, valAndType)
  fields <- forM ps inferProperty
  let ty = TypeApp tyRecord $ rowFromList (map (Label *** snd) fields, REmpty)
  return $ TypedValue sa True (Literal sa (ObjectLiteral (map (fmap (\((expr, ty')) -> TypedValue (exprSourceAnn expr) True expr ty')) fields))) ty
infer' (ObjectUpdate sa o ps) = do
  ensureNoDuplicateProperties ps
  row <- freshType
  newVals <- zipWith (\(name, _) t -> (name, t)) ps <$> traverse (infer . snd) ps
  let newTys = map (\(name, TypedValue _ _ _ ty) -> (Label name, ty)) newVals
  oldTys <- zip (map (Label . fst) ps) <$> replicateM (length ps) freshType
  let oldTy = TypeApp tyRecord $ rowFromList (oldTys, row)
  o' <- TypedValue sa True <$> check o oldTy <*> pure oldTy
  return $ TypedValue sa True (ObjectUpdate sa o' newVals) $ TypeApp tyRecord $ rowFromList (newTys, row)
infer' (Accessor sa prop val) = withErrorMessageHint (ErrorCheckingAccessor val prop) $ do
  field <- freshType
  rest <- freshType
  typed <- check val (TypeApp tyRecord (RCons (Label prop) field rest))
  return $ TypedValue sa True (Accessor sa prop typed) field
infer' (Abs sa binder ret)
  | VarBinder ss arg <- binder = do
      ty <- freshType
      withBindingGroupVisible $ bindLocalVariables [(arg, ty, Defined)] $ do
        body@(TypedValue _ _ _ bodyTy) <- infer' ret
        return $ TypedValue sa True (Abs sa (VarBinder ss arg) body) $ function ty bodyTy
  | otherwise = internalError "Binder was not desugared"
infer' (App sa f arg) = do
  f'@(TypedValue _ _ _ ft) <- infer f
  (ret, app) <- checkFunctionApplication f' ft arg
  return $ TypedValue sa True app ret
infer' (Var sa var) = do
  checkVisibility var
  ty <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards <=< lookupVariable $ var
  case ty of
    ConstrainedType con ty' -> do
      dicts <- getTypeClassDictionaries
      hints <- getHints
      return $ TypedValue sa True (App sa (Var sa var) (TypeClassDictionary sa con dicts hints)) ty'
    _ -> return $ TypedValue sa True (Var sa var) ty
infer' v@(Constructor sa c) = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty, _) -> do (v', ty') <- sndM (introduceSkolemScope <=< replaceAllTypeSynonyms) <=< instantiatePolyTypeWithUnknowns v $ ty
                             return $ TypedValue sa True v' ty'
infer' (Case sa vals binders) = do
  (vals', ts) <- instantiateForBinders vals binders
  ret <- freshType
  binders' <- checkBinders ts ret binders
  return $ TypedValue sa True (Case sa vals' binders') ret
infer' (IfThenElse sa cond th el) = do
  cond' <- check cond tyBoolean
  th'@(TypedValue _ _ _ thTy) <- infer th
  el'@(TypedValue _ _ _ elTy) <- infer el
  (th'', thTy') <- instantiatePolyTypeWithUnknowns th' thTy
  (el'', elTy') <- instantiatePolyTypeWithUnknowns el' elTy
  unifyTypes thTy' elTy'
  return $ TypedValue sa True (IfThenElse sa cond' th'' el'') thTy'
infer' (Let sa ds val) = do
  (ds', val'@(TypedValue _ _ _ valTy)) <- inferLetBinding [] ds val infer
  return $ TypedValue sa True (Let sa ds' val') valTy
infer' (DeferredDictionary sa className tys) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  return $ TypedValue sa False
             (TypeClassDictionary sa (Constraint className tys Nothing) dicts hints)
             (foldl TypeApp (TypeConstructor (fmap coerceProperName className)) tys)
infer' (TypedValue sa checkType val ty) = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  val' <- if checkType then withScopedTypeVars moduleName args (check val ty') else return val
  return $ TypedValue sa True val' ty'
infer' v@(Hole sa name) = do
  ty <- freshType
  ctx <- getLocalContext
  env <- getEnv
  tell . errorMessage $ HoleInferredType name ty ctx (TSBefore env)
  return $ TypedValue sa True v ty
infer' v = traceShow v $ internalError $ "Invalid argument to infer: " ++ show v

inferLetBinding
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [Declaration]
  -> [Declaration]
  -> Expr
  -> (Expr -> m Expr)
  -> m ([Declaration], Expr)
inferLetBinding seen [] ret j = (,) seen <$> withBindingGroupVisible (j ret)
inferLetBinding seen (ValueDecl sa@(ss, _) ident nameKind [] [MkUnguarded ss' tv@(TypedValue sa' checkType val ty)] : rest) ret j =
  warnAndRethrowWithPositionTC ss $ do
    Just moduleName <- checkCurrentModule <$> get
    (kind, args) <- kindOfWithScopedVars ty
    checkTypeKind ty kind
    let dict = M.singleton (Qualified Nothing ident) (ty, nameKind, Undefined)
    ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
    TypedValue _ _ val' ty'' <- if checkType then withScopedTypeVars moduleName args (bindNames dict (check val ty')) else return tv
    bindNames (M.singleton (Qualified Nothing ident) (ty'', nameKind, Defined))
      $ inferLetBinding (seen ++ [ValueDecl sa ident nameKind [] [MkUnguarded ss' (TypedValue sa' checkType val' ty'')]]) rest ret j
inferLetBinding seen (ValueDecl sa@(ss, _) ident nameKind [] [MkUnguarded _ val] : rest) ret j =
  warnAndRethrowWithPositionTC ss $ do
    valTy <- freshType
    let dict = M.singleton (Qualified Nothing ident) (valTy, nameKind, Undefined)
    TypedValue _ _ val' valTy' <- bindNames dict $ infer val
    unifyTypes valTy valTy'
    bindNames (M.singleton (Qualified Nothing ident) (valTy', nameKind, Defined))
      $ inferLetBinding (seen ++ [ValueDecl sa ident nameKind [] [MkUnguarded ss val']]) rest ret j
inferLetBinding seen (BindingGroupDeclaration ds : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  SplitBindingGroup untyped typed dict <- typeDictionaryForBindingGroup Nothing . NEL.toList $ fmap (\(i, _, v) -> (i, v)) ds
  ds1' <- parU typed $ \e -> checkTypedBindingGroupElement moduleName e dict
  ds2' <- forM untyped $ \e -> typeForBindingGroupElement e dict
  let ds' = NEL.fromList [(ident, Private, val') | (ident, (val', _)) <- ds1' ++ ds2']
  bindNames dict $ do
    makeBindingGroupVisible
    inferLetBinding (seen ++ [BindingGroupDeclaration ds']) rest ret j
inferLetBinding _ _ _ _ = internalError "Invalid argument to inferLetBinding"

-- | Infer the types of variables brought into scope by a binder
inferBinder
  :: forall m
   . (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Type
  -> Binder
  -> m (M.Map Ident Type)
inferBinder _ NullBinder{} = return M.empty
inferBinder val (LiteralBinder _ (StringLiteral _)) = unifyTypes val tyString >> return M.empty
inferBinder val (LiteralBinder _ (CharLiteral _)) = unifyTypes val tyChar >> return M.empty
inferBinder val (LiteralBinder _ (NumericLiteral (Left _))) = unifyTypes val tyInt >> return M.empty
inferBinder val (LiteralBinder _ (NumericLiteral (Right _))) = unifyTypes val tyNumber >> return M.empty
inferBinder val (LiteralBinder _ (BooleanLiteral _)) = unifyTypes val tyBoolean >> return M.empty
inferBinder val (VarBinder _ name) = return $ M.singleton name val
inferBinder val (ConstructorBinder _ ctor binders) = do
  env <- getEnv
  case M.lookup ctor (dataConstructors env) of
    Just (_, _, ty, _) -> do
      (_, fn) <- instantiatePolyTypeWithUnknowns (internalError "Data constructor types cannot contain constraints") ty
      fn' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ fn
      let (args, ret) = peelArgs fn'
      unless (length args == length binders) . throwError . errorMessage $ IncorrectConstructorArity ctor
      unifyTypes ret val
      M.unions <$> zipWithM inferBinder (reverse args) binders
    _ -> throwError . errorMessage . UnknownName . fmap DctorName $ ctor
  where
  peelArgs :: Type -> ([Type], Type)
  peelArgs = go []
    where
    go args (TypeApp (TypeApp fn arg) ret) | fn == tyFunction = go (arg : args) ret
    go args ret = (args, ret)
inferBinder val (LiteralBinder _ (ObjectLiteral props)) = do
  row <- freshType
  rest <- freshType
  m1 <- inferRowProperties row rest props
  unifyTypes val (TypeApp tyRecord row)
  return m1
  where
  inferRowProperties :: Type -> Type -> [(PSString, Binder)] -> m (M.Map Ident Type)
  inferRowProperties nrow row [] = unifyTypes nrow row >> return M.empty
  inferRowProperties nrow row ((name, binder):binders) = do
    propTy <- freshType
    m1 <- inferBinder propTy binder
    m2 <- inferRowProperties nrow (RCons (Label name) propTy row) binders
    return $ m1 `M.union` m2
inferBinder val (LiteralBinder _ (ArrayLiteral binders)) = do
  el <- freshType
  m1 <- M.unions <$> traverse (inferBinder el) binders
  unifyTypes val (TypeApp tyArray el)
  return m1
inferBinder val (NamedBinder _ name binder) = do
  m <- inferBinder val binder
  return $ M.insert name val m
inferBinder val (TypedBinder _ ty binder) = do
  kind <- kindOf ty
  checkTypeKind ty kind
  ty1 <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  unifyTypes val ty1
  inferBinder ty1 binder
inferBinder _ OpBinder{} =
  internalError "OpBinder should have been desugared before inferBinder"
inferBinder _ BinaryNoParensBinder{} =
  internalError "BinaryNoParensBinder should have been desugared before inferBinder"
inferBinder _ ParensInBinder{} =
  internalError "ParensInBinder should have been desugared before inferBinder"

-- | Returns true if a binder requires its argument type to be a monotype.
-- | If this is the case, we need to instantiate any polymorphic types before checking binders.
binderRequiresMonotype :: Binder -> Bool
binderRequiresMonotype NullBinder{} = False
binderRequiresMonotype (VarBinder _ _) = False
binderRequiresMonotype (NamedBinder _ _ b) = binderRequiresMonotype b
binderRequiresMonotype (TypedBinder _ ty b) = isMonoType ty || binderRequiresMonotype b
binderRequiresMonotype _ = True

-- | Instantiate polytypes only when necessitated by a binder.
instantiateForBinders
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [Expr]
  -> [CaseAlternative]
  -> m ([Expr], [Type])
instantiateForBinders vals cas = unzip <$> zipWithM (\val inst -> do
  TypedValue _ _ val' ty <- infer val
  if inst
    then instantiatePolyTypeWithUnknowns val' ty
    else return (val', ty)) vals shouldInstantiate
  where
  shouldInstantiate :: [Bool]
  shouldInstantiate = map (any binderRequiresMonotype) . transpose . map caseAlternativeBinders $ cas

-- |
-- Check the types of the return values in a set of binders in a case statement
--
checkBinders
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [Type]
  -> Type
  -> [CaseAlternative]
  -> m [CaseAlternative]
checkBinders _ _ [] = return []
checkBinders nvals ret (CaseAlternative sa binders result : bs) = do
  guardWith (errorMessage' sa $ OverlappingArgNames Nothing) $
    let ns = concatMap binderNames binders in length (ordNub ns) == length ns
  m1 <- M.unions <$> zipWithM inferBinder nvals binders
  r <- bindLocalVariables [ (name, ty, Defined) | (name, ty) <- M.toList m1 ] $
       CaseAlternative sa binders <$> forM result (\ge -> checkGuardedRhs ge ret)
  rs <- checkBinders nvals ret bs
  return $ r : rs

checkGuardedRhs
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => GuardedExpr
  -> Type
  -> m GuardedExpr
checkGuardedRhs (GuardedExpr ss [] rhs) ret = do
  rhs' <- TypedValue (exprSourceAnn rhs) True <$> check rhs ret <*> pure ret
  return $ GuardedExpr ss [] rhs'
checkGuardedRhs (GuardedExpr ss (ConditionGuard ss' cond : guards) rhs) ret = do
  cond' <- withErrorMessageHint ErrorCheckingGuard $ check cond tyBoolean
  GuardedExpr _ guards' rhs' <- checkGuardedRhs (GuardedExpr ss guards rhs) ret
  return $ GuardedExpr ss (ConditionGuard ss' cond' : guards') rhs'
checkGuardedRhs (GuardedExpr ss (PatternGuard ss' binder expr : guards) rhs) ret = do
  expr'@(TypedValue _ _ _ ty) <- infer expr
  variables <- inferBinder ty binder
  GuardedExpr _ guards' rhs' <- bindLocalVariables [ (name, bty, Defined)
                                                    | (name, bty) <- M.toList variables
                                                    ] $
    checkGuardedRhs (GuardedExpr ss guards rhs) ret
  return $ GuardedExpr ss (PatternGuard ss' binder expr' : guards') rhs'

-- |
-- Check the type of a value, rethrowing errors to provide a better error message
--
check
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> Type
  -> m Expr
check val ty = withErrorMessageHint (ErrorCheckingType val ty) $ check' val ty

-- |
-- Check the type of a value
--
check'
  :: forall m
   . (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> Type
  -> m Expr
check' val (ForAll ident ty _) = do
  scope <- newSkolemScope
  sko <- newSkolemConstant
  let sk = skolemize ident sko scope (Just (exprSourceSpan val)) ty
      skVal = skolemizeTypesInValue ident sko scope (exprSourceSpan val) val
  val' <- check skVal sk
  return $ TypedValue (exprSourceAnn val) True val' (ForAll ident ty (Just scope))
check' val t@(ConstrainedType con@(Constraint (Qualified _ (ProperName className)) _ _) ty) = do
  let sa@(ss, _) = exprSourceAnn val
  dictName <- freshIdent ("dict" <> className)
  dicts <- newDictionaries [] (Qualified Nothing dictName) con
  val' <- withBindingGroupVisible $ withTypeClassDictionaries dicts $ check val ty
  return $ TypedValue sa True (Abs sa (VarBinder ss dictName) val') t
check' val u@(TUnknown _) = do
  val'@(TypedValue sa _ _ ty) <- infer val
  -- Don't unify an unknown with an inferred polytype
  (val'', ty') <- instantiatePolyTypeWithUnknowns val' ty
  unifyTypes ty' u
  return $ TypedValue sa True val'' ty'
check' v@(Literal sa (NumericLiteral (Left _))) t | t == tyInt =
  return $ TypedValue sa True v t
check' v@(Literal sa (NumericLiteral (Right _))) t | t == tyNumber =
  return $ TypedValue sa True v t
check' v@(Literal sa (StringLiteral _)) t | t == tyString =
  return $ TypedValue sa True v t
check' v@(Literal sa (CharLiteral _)) t | t == tyChar =
  return $ TypedValue sa True v t
check' v@(Literal sa (BooleanLiteral _)) t | t == tyBoolean =
  return $ TypedValue sa True v t
check' (Literal sa (ArrayLiteral vals)) t@(TypeApp a ty) = do
  unifyTypes a tyArray
  array <- Literal sa . ArrayLiteral <$> forM vals (`check` ty)
  return $ TypedValue sa True array t
check' (Abs sa binder ret) ty@(TypeApp (TypeApp t argTy) retTy)
  | VarBinder ss arg <- binder = do
      unifyTypes t tyFunction
      ret' <- withBindingGroupVisible $ bindLocalVariables [(arg, argTy, Defined)] $ check ret retTy
      return $ TypedValue sa True (Abs sa (VarBinder ss arg) ret') ty
  | otherwise = internalError "Binder was not desugared"
check' (App sa f arg) ret = do
  f'@(TypedValue _ _ _ ft) <- infer f
  (retTy, app) <- checkFunctionApplication f' ft arg
  elaborate <- subsumes retTy ret
  return $ TypedValue sa True (elaborate app) ret
check' v@(Var sa var) ty = do
  checkVisibility var
  repl <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable $ var
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  elaborate <- subsumes repl ty'
  return $ TypedValue sa True (elaborate v) ty'
check' (DeferredDictionary sa className tys) ty = do
  {-
  -- Here, we replace a placeholder for a superclass dictionary with a regular
  -- TypeClassDictionary placeholder. The reason we do this is that it is necessary to have the
  -- correct super instance dictionaries in scope, and these are not available when the type class
  -- declaration gets desugared.
  -}
  dicts <- getTypeClassDictionaries
  hints <- getHints
  return $ TypedValue sa False
             (TypeClassDictionary sa (Constraint className tys Nothing) dicts hints)
             ty
check' (TypedValue sa checkType val ty1) ty2 = do
  kind <- kindOf ty1
  checkTypeKind ty1 kind
  ty1' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty1
  ty2' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty2
  elaborate <- subsumes ty1' ty2'
  val' <- if checkType
            then check val ty1'
            else pure val
  return $ TypedValue sa True (TypedValue sa checkType (elaborate val') ty1') ty2'
check' (Case sa vals binders) ret = do
  (vals', ts) <- instantiateForBinders vals binders
  binders' <- checkBinders ts ret binders
  return $ TypedValue sa True (Case sa vals' binders') ret
check' (IfThenElse sa cond th el) ty = do
  cond' <- check cond tyBoolean
  th' <- check th ty
  el' <- check el ty
  return $ TypedValue sa True (IfThenElse sa cond' th' el') ty
check' e@(Literal sa (ObjectLiteral ps)) t@(TypeApp obj row) | obj == tyRecord = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties e ps row False
  return $ TypedValue sa True (Literal sa (ObjectLiteral ps')) t
check' (TypeClassDictionaryConstructorApp sa name ps) t = do
  ps' <- check' ps t
  return $ TypedValue sa True (TypeClassDictionaryConstructorApp sa name ps') t
check' e@(ObjectUpdate sa obj ps) t@(TypeApp o row) | o == tyRecord = do
  ensureNoDuplicateProperties ps
  -- We need to be careful to avoid duplicate labels here.
  -- We check _obj_ against the type _t_ with the types in _ps_ replaced with unknowns.
  let (propsToCheck, rest) = rowToList row
      (removedProps, remainingProps) = partition (\(p, _) -> p `elem` map (Label . fst) ps) propsToCheck
  us <- zip (map fst removedProps) <$> replicateM (length ps) freshType
  obj' <- check obj (TypeApp tyRecord (rowFromList (us ++ remainingProps, rest)))
  ps' <- checkProperties e ps row True
  return $ TypedValue sa True (ObjectUpdate sa obj' ps') t
check' (Accessor sa prop val) ty = withErrorMessageHint (ErrorCheckingAccessor val prop) $ do
  rest <- freshType
  val' <- check val (TypeApp tyRecord (RCons (Label prop) ty rest))
  return $ TypedValue sa True (Accessor sa prop val') ty
check' v@(Constructor sa c) ty = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty1, _) -> do
      repl <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
      ty' <- introduceSkolemScope ty
      elaborate <- subsumes repl ty'
      return $ TypedValue sa True (elaborate v) ty'
check' (Let sa ds val) ty = do
  (ds', val') <- inferLetBinding [] ds val (`check` ty)
  return $ TypedValue sa True (Let sa ds' val') ty
check' val kt@(KindedType ty kind) = do
  checkTypeKind ty kind
  val' <- check' val ty
  return $ TypedValue (exprSourceAnn val) True val' kt
check' val ty = do
  TypedValue sa _ val' ty' <- infer val
  elaborate <- subsumes ty' ty
  return $ TypedValue sa True (elaborate val') ty

-- |
-- Check the type of a collection of named record fields
--
-- The @lax@ parameter controls whether or not every record member has to be provided. For object updates, this is not the case.
--
checkProperties
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> [(PSString, Expr)]
  -> Type
  -> Bool
  -> m [(PSString, Expr)]
checkProperties expr ps row lax = let (ts, r') = rowToList row in go ps ts r' where
  go [] [] REmpty = return []
  go [] [] u@(TUnknown _)
    | lax = return []
    | otherwise = do unifyTypes u REmpty
                     return []
  go [] [] Skolem{} | lax = return []
  go [] ((p, _): _) _ | lax = return []
                      | otherwise = throwError . errorMessage $ PropertyIsMissing p
  go ((p,_):_) [] REmpty = throwError . errorMessage $ AdditionalProperty $ Label p
  go ((p,v):ps') ts r =
    case lookup (Label p) ts of
      Nothing -> do
        v'@(TypedValue _ _ _ ty) <- infer v
        rest <- freshType
        unifyTypes r (RCons (Label p) ty rest)
        ps'' <- go ps' ts rest
        return $ (p, v') : ps''
      Just ty -> do
        v' <- check v ty
        ps'' <- go ps' (delete (Label p, ty) ts) r
        return $ (p, v') : ps''
  go _ _ _ = throwError . errorMessage $ ExprDoesNotHaveType expr (TypeApp tyRecord row)

-- | Check the type of a function application, rethrowing errors to provide a better error message.
--
-- This judgment takes three inputs:
--
-- * The expression of the function we are applying
-- * The type of that function
-- * The expression we are applying it to
--
-- and synthesizes two outputs:
--
-- * The return type
-- * The elaborated expression for the function application (since we might need to
--   insert type class dictionaries, etc.)
checkFunctionApplication
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -- ^ The function expression
  -> Type
  -- ^ The type of the function
  -> Expr
  -- ^ The argument expression
  -> m (Type, Expr)
  -- ^ The result type, and the elaborated term
checkFunctionApplication fn fnTy arg = withErrorMessageHint (ErrorInApplication fn fnTy arg) $ do
  subst <- gets checkSubstitution
  checkFunctionApplication' fn (substituteType subst fnTy) arg

-- | Check the type of a function application
checkFunctionApplication'
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> Type
  -> Expr
  -> m (Type, Expr)
checkFunctionApplication' fn (TypeApp (TypeApp tyFunction' argTy) retTy) arg = do
  unifyTypes tyFunction' tyFunction
  arg' <- check arg argTy
  return (retTy, App (exprSourceAnn fn) fn arg')
checkFunctionApplication' fn (ForAll ident ty _) arg = do
  replaced <- replaceVarWithUnknown ident ty
  checkFunctionApplication fn replaced arg
checkFunctionApplication' fn (KindedType ty _) arg =
  checkFunctionApplication fn ty arg
checkFunctionApplication' fn (ConstrainedType con fnTy) arg = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  checkFunctionApplication' (App (exprSourceAnn fn) fn (TypeClassDictionary (exprSourceAnn fn) con dicts hints)) fnTy arg
checkFunctionApplication' fn fnTy dict@TypeClassDictionary{} =
  return (fnTy, App (exprSourceAnn fn) fn dict)
checkFunctionApplication' fn u arg = do
  arg' <- do
    TypedValue sa _ arg' t <- infer arg
    (arg'', t') <- instantiatePolyTypeWithUnknowns arg' t
    return $ TypedValue sa True arg'' t'
  let ty = (\(TypedValue _ _ _ t) -> t) arg'
  ret <- freshType
  unifyTypes u (function ty ret)
  return (ret, App (exprSourceAnn fn) fn arg')

-- |
-- Ensure a set of property names and value does not contain duplicate labels
--
ensureNoDuplicateProperties :: (MonadError MultipleErrors m) => [(PSString, Expr)] -> m ()
ensureNoDuplicateProperties ps =
  let ls = map fst ps in
  case ls \\ ordNub ls of
    l : _ -> throwError . errorMessage $ DuplicateLabel (Label l) Nothing
    _ -> return ()
