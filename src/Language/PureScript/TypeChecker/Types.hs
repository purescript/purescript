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

import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.List (transpose, (\\), partition, delete)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (for)

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
  -> [(Ident, Expr)]
  -> m [(Ident, (Expr, Type))]
typesOf bindingGroupType moduleName vals = withFreshSubstitution $ do
    (tys, wInfer) <- capturingSubstitution tidyUp $ do
      (SplitBindingGroup untyped typed dict, w) <- withoutWarnings $ typeDictionaryForBindingGroup (Just moduleName) vals
      ds1 <- parU typed $ \e -> withoutWarnings $ checkTypedBindingGroupElement moduleName e dict
      ds2 <- forM untyped $ \e -> withoutWarnings $ typeForBindingGroupElement e dict
      return (map (False, ) ds1 ++ map (True, ) ds2, w)

    inferred <- forM tys $ \(shouldGeneralize, ((ident, (val, ty)), _)) -> do
      -- Replace type class dictionary placeholders with actual dictionaries
      (val', unsolved) <- replaceTypeClassDictionaries shouldGeneralize val
      -- Generalize and constrain the type
      currentSubst <- gets checkSubstitution
      let ty' = substituteType currentSubst ty
          unsolvedTypeVars = ordNub $ unknownsInType ty'
          generalized = generalize unsolved ty'

      when shouldGeneralize $ do
        -- Show the inferred type in a warning
        tell . errorMessage $ MissingTypeDeclaration ident generalized
        -- For non-recursive binding groups, can generalize over constraints.
        -- For recursive binding groups, we throw an error here for now.
        when (bindingGroupType == RecursiveBindingGroup && not (null unsolved))
          . throwError
          . errorMessage
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
          when (any (`notElem` unsolvedTypeVars) constraintTypeVars) $ do
            throwError . onErrorMessages (replaceTypes currentSubst) . errorMessage $ AmbiguousTypeVariables generalized con

      -- Check skolem variables did not escape their scope
      skolemEscapeCheck val'
      return ((ident, (foldr (Abs . VarBinder . (\(x, _, _) -> x)) val' unsolved, generalized)), unsolved)

    -- Show warnings here, since types in wildcards might have been solved during
    -- instance resolution (by functional dependencies).
    finalState <- get
    let replaceTypes' = replaceTypes (checkSubstitution finalState)
        runTypeSearch' gen = runTypeSearch (guard gen $> foldMap snd inferred) finalState
        raisePreviousWarnings gen w = (escalateWarningWhen isHoleError . tell . onErrorMessages (runTypeSearch' gen . replaceTypes')) w

    raisePreviousWarnings False wInfer
    forM_ tys $ \(shouldGeneralize, ((_, (_, _)), w)) -> do
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
  { _splitBindingGroupUntyped :: [(Ident, (Expr, Type))]
  -- ^ The untyped expressions
  , _splitBindingGroupTyped :: [(Ident, (Expr, Type, Bool))]
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
  -> [(Ident, Expr)]
  -> m SplitBindingGroup
typeDictionaryForBindingGroup moduleName vals = do
    -- Filter the typed and untyped declarations and make a map of names to typed declarations.
    -- Replace type wildcards here so that the resulting dictionary of types contains the
    -- fully expanded types.
    let (untyped, typed) = partitionEithers (map splitTypeAnnotation vals)
    (typedDict, typed') <- fmap unzip . for typed $ \(ident, (expr, ty, checkType)) -> do
      ty' <- replaceTypeWildcards ty
      return ((ident, ty'), (ident, (expr, ty', checkType)))
    -- Create fresh unification variables for the types of untyped declarations
    (untypedDict, untyped') <- fmap unzip . for untyped $ \(ident, expr) -> do
      ty <- freshType
      return ((ident, ty), (ident, (expr, ty)))
    -- Create the dictionary of all name/type pairs, which will be added to the
    -- environment during type checking
    let dict = M.fromList [ (Qualified moduleName ident, (ty, Private, Undefined))
                          | (ident, ty) <- typedDict <> untypedDict
                          ]
    return (SplitBindingGroup untyped' typed' dict)
  where
    -- | Check if a value contains a type annotation, and if so, separate it
    -- from the value itself.
    splitTypeAnnotation :: (Ident, Expr) -> Either (Ident, Expr) (Ident, (Expr, Type, Bool))
    splitTypeAnnotation (name, TypedValue checkType value ty) = Right (name, (value, ty, checkType))
    splitTypeAnnotation (name, PositionedValue pos c value) =
      bimap (second (PositionedValue pos c))
            (second (\(e, t, b) -> (PositionedValue pos c e, t, b)))
            (splitTypeAnnotation (name, value))
    splitTypeAnnotation (name, value) = Left (name, value)

-- | Check the type annotation of a typed value in a binding group.
checkTypedBindingGroupElement
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> (Ident, (Expr, Type, Bool))
  -- ^ The identifier we are trying to define, along with the expression and its type annotation
  -> M.Map (Qualified Ident) (Type, NameKind, NameVisibility)
  -- ^ Names brought into scope in this binding group
  -> m (Ident, (Expr, Type))
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
            else return (TypedValue False val ty')
  return (ident, (val', ty'))

-- | Infer a type for a value in a binding group which lacks an annotation.
typeForBindingGroupElement
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => (Ident, (Expr, Type))
  -- ^ The identifier we are trying to define, along with the expression and its assigned type
  -- (at this point, this should be a unification variable)
  -> M.Map (Qualified Ident) (Type, NameKind, NameVisibility)
  -- ^ Names brought into scope in this binding group
  -> m (Ident, (Expr, Type))
typeForBindingGroupElement (ident, (val, ty)) dict = do
  -- Infer the type with the new names in scope
  TypedValue _ val' ty' <- bindNames dict $ infer val
  -- Unify the type with the unification variable we chose for this definition
  unifyTypes ty ty'
  return (ident, (TypedValue True val' ty', ty'))

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
   instantiatePolyTypeWithUnknowns (App val (TypeClassDictionary con dicts hints)) ty
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
infer' v@(Literal (NumericLiteral (Left _))) = return $ TypedValue True v tyInt
infer' v@(Literal (NumericLiteral (Right _))) = return $ TypedValue True v tyNumber
infer' v@(Literal (StringLiteral _)) = return $ TypedValue True v tyString
infer' v@(Literal (CharLiteral _)) = return $ TypedValue True v tyChar
infer' v@(Literal (BooleanLiteral _)) = return $ TypedValue True v tyBoolean
infer' (Literal (ArrayLiteral vals)) = do
  ts <- traverse infer vals
  els <- freshType
  ts' <- forM ts $ \(TypedValue ch val t) -> do
    (val', t') <- instantiatePolyTypeWithUnknowns val t
    unifyTypes els t'
    return (TypedValue ch val' t')
  return $ TypedValue True (Literal (ArrayLiteral ts')) (TypeApp tyArray els)
infer' (Literal (ObjectLiteral ps)) = do
  ensureNoDuplicateProperties ps
  -- We make a special case for Vars in record labels, since these are the
  -- only types of expressions for which 'infer' can return a polymorphic type.
  -- They need to be instantiated here.
  let shouldInstantiate :: Expr -> Bool
      shouldInstantiate Var{} = True
      shouldInstantiate (PositionedValue _ _ e) = shouldInstantiate e
      shouldInstantiate _ = False

      inferProperty :: (PSString, Expr) -> m (PSString, (Expr, Type))
      inferProperty (name, val) = do
        TypedValue _ val' ty <- infer val
        valAndType <- if shouldInstantiate val
                        then instantiatePolyTypeWithUnknowns val' ty
                        else pure (val', ty)
        pure (name, valAndType)
  fields <- forM ps inferProperty
  let ty = TypeApp tyRecord $ rowFromList (map (Label *** snd) fields, REmpty)
  return $ TypedValue True (Literal (ObjectLiteral (map (fmap (uncurry (TypedValue True))) fields))) ty
infer' (ObjectUpdate o ps) = do
  ensureNoDuplicateProperties ps
  row <- freshType
  newVals <- zipWith (\(name, _) t -> (name, t)) ps <$> traverse (infer . snd) ps
  let newTys = map (\(name, TypedValue _ _ ty) -> (Label name, ty)) newVals
  oldTys <- zip (map (Label . fst) ps) <$> replicateM (length ps) freshType
  let oldTy = TypeApp tyRecord $ rowFromList (oldTys, row)
  o' <- TypedValue True <$> check o oldTy <*> pure oldTy
  return $ TypedValue True (ObjectUpdate o' newVals) $ TypeApp tyRecord $ rowFromList (newTys, row)
infer' (Accessor prop val) = withErrorMessageHint (ErrorCheckingAccessor val prop) $ do
  field <- freshType
  rest <- freshType
  typed <- check val (TypeApp tyRecord (RCons (Label prop) field rest))
  return $ TypedValue True (Accessor prop typed) field
infer' (Abs binder ret)
  | VarBinder arg <- binder = do
      ty <- freshType
      withBindingGroupVisible $ bindLocalVariables [(arg, ty, Defined)] $ do
        body@(TypedValue _ _ bodyTy) <- infer' ret
        return $ TypedValue True (Abs (VarBinder arg) body) $ function ty bodyTy
  | otherwise = internalError "Binder was not desugared"
infer' (App f arg) = do
  f'@(TypedValue _ _ ft) <- infer f
  (ret, app) <- checkFunctionApplication f' ft arg
  return $ TypedValue True app ret
infer' (Var var) = do
  checkVisibility var
  ty <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards <=< lookupVariable $ var
  case ty of
    ConstrainedType con ty' -> do
      dicts <- getTypeClassDictionaries
      hints <- getHints
      return $ TypedValue True (App (Var var) (TypeClassDictionary con dicts hints)) ty'
    _ -> return $ TypedValue True (Var var) ty
infer' v@(Constructor c) = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty, _) -> do (v', ty') <- sndM (introduceSkolemScope <=< replaceAllTypeSynonyms) <=< instantiatePolyTypeWithUnknowns v $ ty
                             return $ TypedValue True v' ty'
infer' (Case vals binders) = do
  (vals', ts) <- instantiateForBinders vals binders
  ret <- freshType
  binders' <- checkBinders ts ret binders
  return $ TypedValue True (Case vals' binders') ret
infer' (IfThenElse cond th el) = do
  cond' <- check cond tyBoolean
  th'@(TypedValue _ _ thTy) <- infer th
  el'@(TypedValue _ _ elTy) <- infer el
  (th'', thTy') <- instantiatePolyTypeWithUnknowns th' thTy
  (el'', elTy') <- instantiatePolyTypeWithUnknowns el' elTy
  unifyTypes thTy' elTy'
  return $ TypedValue True (IfThenElse cond' th'' el'') thTy'
infer' (Let ds val) = do
  (ds', val'@(TypedValue _ _ valTy)) <- inferLetBinding [] ds val infer
  return $ TypedValue True (Let ds' val') valTy
infer' (DeferredDictionary className tys) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  return $ TypedValue False
             (TypeClassDictionary (Constraint className tys Nothing) dicts hints)
             (foldl TypeApp (TypeConstructor (fmap coerceProperName className)) tys)
infer' (TypedValue checkType val ty) = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  val' <- if checkType then withScopedTypeVars moduleName args (check val ty') else return val
  return $ TypedValue True val' ty'
infer' (Hole name) = do
  ty <- freshType
  ctx <- getLocalContext
  env <- getEnv
  tell . errorMessage $ HoleInferredType name ty ctx (TSBefore env)
  return $ TypedValue True (Hole name) ty
infer' (PositionedValue pos c val) = warnAndRethrowWithPositionTC pos $ do
  TypedValue t v ty <- infer' val
  return $ TypedValue t (PositionedValue pos c v) ty
infer' v = internalError $ "Invalid argument to infer: " ++ show v

inferLetBinding
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [Declaration]
  -> [Declaration]
  -> Expr
  -> (Expr -> m Expr)
  -> m ([Declaration], Expr)
inferLetBinding seen [] ret j = (,) seen <$> withBindingGroupVisible (j ret)
inferLetBinding seen (ValueDeclaration ident nameKind [] [MkUnguarded tv@(TypedValue checkType val ty)] : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  let dict = M.singleton (Qualified Nothing ident) (ty, nameKind, Undefined)
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  TypedValue _ val' ty'' <- if checkType then withScopedTypeVars moduleName args (bindNames dict (check val ty')) else return tv
  bindNames (M.singleton (Qualified Nothing ident) (ty'', nameKind, Defined)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] [MkUnguarded (TypedValue checkType val' ty'')]]) rest ret j
inferLetBinding seen (ValueDeclaration ident nameKind [] [MkUnguarded val] : rest) ret j = do
  valTy <- freshType
  let dict = M.singleton (Qualified Nothing ident) (valTy, nameKind, Undefined)
  TypedValue _ val' valTy' <- bindNames dict $ infer val
  unifyTypes valTy valTy'
  bindNames (M.singleton (Qualified Nothing ident) (valTy', nameKind, Defined)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] [MkUnguarded val']]) rest ret j
inferLetBinding seen (BindingGroupDeclaration ds : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  SplitBindingGroup untyped typed dict <- typeDictionaryForBindingGroup Nothing (map (\(i, _, v) -> (i, v)) ds)
  ds1' <- parU typed $ \e -> checkTypedBindingGroupElement moduleName e dict
  ds2' <- forM untyped $ \e -> typeForBindingGroupElement e dict
  let ds' = [(ident, Private, val') | (ident, (val', _)) <- ds1' ++ ds2']
  bindNames dict $ do
    makeBindingGroupVisible
    inferLetBinding (seen ++ [BindingGroupDeclaration ds']) rest ret j
inferLetBinding seen (PositionedDeclaration pos com d : ds) ret j = warnAndRethrowWithPositionTC pos $ do
  (d' : ds', val') <- inferLetBinding seen (d : ds) ret j
  return (PositionedDeclaration pos com d' : ds', val')
inferLetBinding _ _ _ _ = internalError "Invalid argument to inferLetBinding"

-- | Infer the types of variables brought into scope by a binder
inferBinder
  :: forall m
   . (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Type
  -> Binder
  -> m (M.Map Ident Type)
inferBinder _ NullBinder = return M.empty
inferBinder val (LiteralBinder (StringLiteral _)) = unifyTypes val tyString >> return M.empty
inferBinder val (LiteralBinder (CharLiteral _)) = unifyTypes val tyChar >> return M.empty
inferBinder val (LiteralBinder (NumericLiteral (Left _))) = unifyTypes val tyInt >> return M.empty
inferBinder val (LiteralBinder (NumericLiteral (Right _))) = unifyTypes val tyNumber >> return M.empty
inferBinder val (LiteralBinder (BooleanLiteral _)) = unifyTypes val tyBoolean >> return M.empty
inferBinder val (VarBinder name) = return $ M.singleton name val
inferBinder val (ConstructorBinder ctor binders) = do
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
inferBinder val (LiteralBinder (ObjectLiteral props)) = do
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
inferBinder val (LiteralBinder (ArrayLiteral binders)) = do
  el <- freshType
  m1 <- M.unions <$> traverse (inferBinder el) binders
  unifyTypes val (TypeApp tyArray el)
  return m1
inferBinder val (NamedBinder name binder) = do
  m <- inferBinder val binder
  return $ M.insert name val m
inferBinder val (PositionedBinder pos _ binder) =
  warnAndRethrowWithPositionTC pos $ inferBinder val binder
inferBinder val (TypedBinder ty binder) = do
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
binderRequiresMonotype NullBinder = False
binderRequiresMonotype (VarBinder _) = False
binderRequiresMonotype (NamedBinder _ b) = binderRequiresMonotype b
binderRequiresMonotype (PositionedBinder _ _ b) = binderRequiresMonotype b
binderRequiresMonotype (TypedBinder ty b) = isMonoType ty || binderRequiresMonotype b
binderRequiresMonotype _ = True

-- | Instantiate polytypes only when necessitated by a binder.
instantiateForBinders
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [Expr]
  -> [CaseAlternative]
  -> m ([Expr], [Type])
instantiateForBinders vals cas = unzip <$> zipWithM (\val inst -> do
  TypedValue _ val' ty <- infer val
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
checkBinders nvals ret (CaseAlternative binders result : bs) = do
  guardWith (errorMessage $ OverlappingArgNames Nothing) $
    let ns = concatMap binderNames binders in length (ordNub ns) == length ns
  m1 <- M.unions <$> zipWithM inferBinder nvals binders
  r <- bindLocalVariables [ (name, ty, Defined) | (name, ty) <- M.toList m1 ] $
       CaseAlternative binders <$> forM result (\ge -> checkGuardedRhs ge ret)
  rs <- checkBinders nvals ret bs
  return $ r : rs

checkGuardedRhs
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => GuardedExpr
  -> Type
  -> m GuardedExpr
checkGuardedRhs (GuardedExpr [] rhs) ret = do
  rhs' <- TypedValue True <$> check rhs ret <*> pure ret
  return $ GuardedExpr [] rhs'
checkGuardedRhs (GuardedExpr (ConditionGuard cond : guards) rhs) ret = do
  cond' <- withErrorMessageHint ErrorCheckingGuard $ check cond tyBoolean
  GuardedExpr guards' rhs' <- checkGuardedRhs (GuardedExpr guards rhs) ret
  return $ GuardedExpr (ConditionGuard cond' : guards') rhs'
checkGuardedRhs (GuardedExpr (PatternGuard binder expr : guards) rhs) ret = do
  expr'@(TypedValue _ _ ty) <- infer expr
  variables <- inferBinder ty binder
  GuardedExpr guards' rhs' <- bindLocalVariables [ (name, bty, Defined)
                                                 | (name, bty) <- M.toList variables
                                                 ] $
    checkGuardedRhs (GuardedExpr guards rhs) ret
  return $ GuardedExpr (PatternGuard binder expr' : guards') rhs'

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
  let ss = case val of
             PositionedValue pos _ _ -> Just pos
             _ -> Nothing
      sk = skolemize ident sko scope ss ty
      skVal = skolemizeTypesInValue ident sko scope ss val
  val' <- check skVal sk
  return $ TypedValue True val' (ForAll ident ty (Just scope))
check' val t@(ConstrainedType con@(Constraint (Qualified _ (ProperName className)) _ _) ty) = do
  dictName <- freshIdent ("dict" <> className)
  dicts <- newDictionaries [] (Qualified Nothing dictName) con
  val' <- withBindingGroupVisible $ withTypeClassDictionaries dicts $ check val ty
  return $ TypedValue True (Abs (VarBinder dictName) val') t
check' val u@(TUnknown _) = do
  val'@(TypedValue _ _ ty) <- infer val
  -- Don't unify an unknown with an inferred polytype
  (val'', ty') <- instantiatePolyTypeWithUnknowns val' ty
  unifyTypes ty' u
  return $ TypedValue True val'' ty'
check' v@(Literal (NumericLiteral (Left _))) t | t == tyInt =
  return $ TypedValue True v t
check' v@(Literal (NumericLiteral (Right _))) t | t == tyNumber =
  return $ TypedValue True v t
check' v@(Literal (StringLiteral _)) t | t == tyString =
  return $ TypedValue True v t
check' v@(Literal (CharLiteral _)) t | t == tyChar =
  return $ TypedValue True v t
check' v@(Literal (BooleanLiteral _)) t | t == tyBoolean =
  return $ TypedValue True v t
check' (Literal (ArrayLiteral vals)) t@(TypeApp a ty) = do
  unifyTypes a tyArray
  array <- Literal . ArrayLiteral <$> forM vals (`check` ty)
  return $ TypedValue True array t
check' (Abs binder ret) ty@(TypeApp (TypeApp t argTy) retTy)
  | VarBinder arg <- binder = do
      unifyTypes t tyFunction
      ret' <- withBindingGroupVisible $ bindLocalVariables [(arg, argTy, Defined)] $ check ret retTy
      return $ TypedValue True (Abs (VarBinder arg) ret') ty
  | otherwise = internalError "Binder was not desugared"
check' (App f arg) ret = do
  f'@(TypedValue _ _ ft) <- infer f
  (retTy, app) <- checkFunctionApplication f' ft arg
  elaborate <- subsumes retTy ret
  return $ TypedValue True (elaborate app) ret
check' v@(Var var) ty = do
  checkVisibility var
  repl <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable $ var
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  elaborate <- subsumes repl ty'
  return $ TypedValue True (elaborate v) ty'
check' (DeferredDictionary className tys) ty = do
  {-
  -- Here, we replace a placeholder for a superclass dictionary with a regular
  -- TypeClassDictionary placeholder. The reason we do this is that it is necessary to have the
  -- correct super instance dictionaries in scope, and these are not available when the type class
  -- declaration gets desugared.
  -}
  dicts <- getTypeClassDictionaries
  hints <- getHints
  return $ TypedValue False
             (TypeClassDictionary (Constraint className tys Nothing) dicts hints)
             ty
check' (TypedValue checkType val ty1) ty2 = do
  kind <- kindOf ty1
  checkTypeKind ty1 kind
  ty1' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty1
  ty2' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty2
  elaborate <- subsumes ty1' ty2'
  val' <- if checkType
            then check val ty1'
            else pure val
  return $ TypedValue True (TypedValue checkType (elaborate val') ty1') ty2'
check' (Case vals binders) ret = do
  (vals', ts) <- instantiateForBinders vals binders
  binders' <- checkBinders ts ret binders
  return $ TypedValue True (Case vals' binders') ret
check' (IfThenElse cond th el) ty = do
  cond' <- check cond tyBoolean
  th' <- check th ty
  el' <- check el ty
  return $ TypedValue True (IfThenElse cond' th' el') ty
check' e@(Literal (ObjectLiteral ps)) t@(TypeApp obj row) | obj == tyRecord = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties e ps row False
  return $ TypedValue True (Literal (ObjectLiteral ps')) t
check' (TypeClassDictionaryConstructorApp name ps) t = do
  ps' <- check' ps t
  return $ TypedValue True (TypeClassDictionaryConstructorApp name ps') t
check' e@(ObjectUpdate obj ps) t@(TypeApp o row) | o == tyRecord = do
  ensureNoDuplicateProperties ps
  -- We need to be careful to avoid duplicate labels here.
  -- We check _obj_ against the type _t_ with the types in _ps_ replaced with unknowns.
  let (propsToCheck, rest) = rowToList row
      (removedProps, remainingProps) = partition (\(p, _) -> p `elem` map (Label . fst) ps) propsToCheck
  us <- zip (map fst removedProps) <$> replicateM (length ps) freshType
  obj' <- check obj (TypeApp tyRecord (rowFromList (us ++ remainingProps, rest)))
  ps' <- checkProperties e ps row True
  return $ TypedValue True (ObjectUpdate obj' ps') t
check' (Accessor prop val) ty = withErrorMessageHint (ErrorCheckingAccessor val prop) $ do
  rest <- freshType
  val' <- check val (TypeApp tyRecord (RCons (Label prop) ty rest))
  return $ TypedValue True (Accessor prop val') ty
check' v@(Constructor c) ty = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty1, _) -> do
      repl <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
      ty' <- introduceSkolemScope ty
      elaborate <- subsumes repl ty'
      return $ TypedValue True (elaborate v) ty'
check' (Let ds val) ty = do
  (ds', val') <- inferLetBinding [] ds val (`check` ty)
  return $ TypedValue True (Let ds' val') ty
check' val kt@(KindedType ty kind) = do
  checkTypeKind ty kind
  val' <- check' val ty
  return $ TypedValue True val' kt
check' (PositionedValue pos c val) ty = warnAndRethrowWithPositionTC pos $ do
  TypedValue t v ty' <- check' val ty
  return $ TypedValue t (PositionedValue pos c v) ty'
check' val ty = do
  TypedValue _ val' ty' <- infer val
  elaborate <- subsumes ty' ty
  return $ TypedValue True (elaborate val') ty

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
        v'@(TypedValue _ _ ty) <- infer v
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
  return (retTy, App fn arg')
checkFunctionApplication' fn (ForAll ident ty _) arg = do
  replaced <- replaceVarWithUnknown ident ty
  checkFunctionApplication fn replaced arg
checkFunctionApplication' fn (KindedType ty _) arg =
  checkFunctionApplication fn ty arg
checkFunctionApplication' fn (ConstrainedType con fnTy) arg = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  checkFunctionApplication' (App fn (TypeClassDictionary con dicts hints)) fnTy arg
checkFunctionApplication' fn fnTy dict@TypeClassDictionary{} =
  return (fnTy, App fn dict)
checkFunctionApplication' fn u arg = do
  arg' <- do
    TypedValue _ arg' t <- infer arg
    (arg'', t') <- instantiatePolyTypeWithUnknowns arg' t
    return $ TypedValue True arg'' t'
  let ty = (\(TypedValue _ _ t) -> t) arg'
  ret <- freshType
  unifyTypes u (function ty ret)
  return (ret, App fn arg')

-- |
-- Ensure a set of property names and value does not contain duplicate labels
--
ensureNoDuplicateProperties :: (MonadError MultipleErrors m) => [(PSString, Expr)] -> m ()
ensureNoDuplicateProperties ps =
  let ls = map fst ps in
  case ls \\ ordNub ls of
    l : _ -> throwError . errorMessage $ DuplicateLabel (Label l) Nothing
    _ -> return ()
