{-# LANGUAGE OverloadedStrings #-}

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

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Either (lefts, rights)
import Data.List (transpose, nub, (\\), partition, delete)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

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
import Language.PureScript.TypeChecker.Rows
import Language.PureScript.TypeChecker.Skolems
import Language.PureScript.TypeChecker.Subsumption
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

data BindingGroupType
  = RecursiveBindingGroup
  | NonRecursiveBindingGroup
  deriving (Show, Eq, Ord)

-- | Infer the types of multiple mutually-recursive values, and return elaborated values including
-- type class dictionaries and type annotations.
typesOf ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  BindingGroupType ->
  ModuleName ->
  [(Ident, Expr)] ->
  m [(Ident, (Expr, Type))]
typesOf bindingGroupType moduleName vals = do
  tys <- fmap tidyUp . escalateWarningWhen isHoleError . liftUnifyWarnings replace $ do
    (untyped, typed, dict, untypedDict) <- typeDictionaryForBindingGroup moduleName vals
    ds1 <- parU typed $ \e -> checkTypedBindingGroupElement moduleName e dict
    ds2 <- forM untyped $ \e -> typeForBindingGroupElement e dict untypedDict
    return (map (\x -> (False, x)) ds1 ++ map (\x -> (True, x)) ds2)

  forM tys $ \(shouldGeneralize, (ident, (val, ty))) -> do
    -- Replace type class dictionary placeholders with actual dictionaries
    (val', unsolved) <- replaceTypeClassDictionaries shouldGeneralize moduleName val
    let unsolvedTypeVars = nub $ unknownsInType ty
    -- Generalize and constrain the type
    let generalized = generalize unsolved ty

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
      forM_ unsolved $ \(_, con) -> do
        let constraintTypeVars = nub $ foldMap unknownsInType (constraintArgs con)
        when (any (`notElem` unsolvedTypeVars) constraintTypeVars) $
          throwError . errorMessage $ NoInstanceFound con

    -- Check skolem variables did not escape their scope
    skolemEscapeCheck val'
    -- Check rows do not contain duplicate labels
    checkDuplicateLabels val'
    return (ident, (foldr (Abs . Left . fst) val' unsolved, generalized))
  where

  -- | Generalize type vars using forall and add inferred constraints
  generalize unsolved = varIfUnknown . constrain unsolved

  -- | Add any unsolved constraints
  constrain [] = id
  constrain cs = ConstrainedType (map snd cs)

  -- Apply the substitution that was returned from runUnify to both types and (type-annotated) values
  tidyUp (ts, sub) = map (\(b, (i, (val, ty))) -> (b, (i, (overTypes (substituteType sub) val, substituteType sub ty)))) ts

  -- Replace all the wildcards types with their inferred types
  replace sub (ErrorMessage hints (WildcardInferredType ty)) =
    ErrorMessage hints . WildcardInferredType $ substituteType sub ty
  replace sub (ErrorMessage hints (HoleInferredType name ty env)) =
    ErrorMessage hints $ HoleInferredType name (substituteType sub ty)
                                               (map (second (substituteType sub)) env)
  replace _ em = em

  isHoleError :: ErrorMessage -> Bool
  isHoleError (ErrorMessage _ HoleInferredType{}) = True
  isHoleError _ = False

type TypeData = M.Map (ModuleName, Ident) (Type, NameKind, NameVisibility)

type UntypedData = [(Ident, Type)]

typeDictionaryForBindingGroup ::
  (MonadState CheckState m) =>
  ModuleName ->
  [(Ident, Expr)] ->
  m ([(Ident, Expr)], [(Ident, (Expr, Type, Bool))], TypeData, UntypedData)
typeDictionaryForBindingGroup moduleName vals = do
  let
    -- Map each declaration to a name/value pair, with an optional type, if the declaration is typed
    es = map isTyped vals
    -- Filter the typed and untyped declarations
    untyped = lefts es
    typed = rights es
    -- Make a map of names to typed declarations
    typedDict = map (\(ident, (_, ty, _)) -> (ident, ty)) typed

  -- Create fresh unification variables for the types of untyped declarations
  untypedNames <- replicateM (length untyped) freshType

  let
    -- Make a map of names to the unification variables of untyped declarations
    untypedDict = zip (map fst untyped) untypedNames
    -- Create the dictionary of all name/type pairs, which will be added to the environment during type checking
    dict = M.fromList (map (\(ident, ty) -> ((moduleName, ident), (ty, Private, Undefined))) $ typedDict ++ untypedDict)
  return (untyped, typed, dict, untypedDict)

checkTypedBindingGroupElement ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  ModuleName ->
  (Ident, (Expr, Type, Bool)) ->
  TypeData ->
  m (Ident, (Expr, Type))
checkTypedBindingGroupElement mn (ident, (val', ty, checkType)) dict = do
  -- Replace type wildcards
  ty' <- replaceTypeWildcards ty
  -- Kind check
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  -- Check the type with the new names in scope
  ty'' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty'
  val'' <- if checkType
           then withScopedTypeVars mn args $ bindNames dict $ TypedValue True <$> check val' ty'' <*> pure ty''
           else return (TypedValue False val' ty'')
  return (ident, (val'', ty''))

typeForBindingGroupElement ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  (Ident, Expr) ->
  TypeData ->
  UntypedData ->
  m (Ident, (Expr, Type))
typeForBindingGroupElement (ident, val) dict untypedDict = do
  -- Infer the type with the new names in scope
  TypedValue _ val' ty <- bindNames dict $ infer val
  unifyTypes ty $ fromMaybe (internalError "name not found in dictionary") (lookup ident untypedDict)
  return (ident, (TypedValue True val' ty, ty))

-- | Check if a value contains a type annotation
isTyped :: (Ident, Expr) -> Either (Ident, Expr) (Ident, (Expr, Type, Bool))
isTyped (name, TypedValue checkType value ty) = Right (name, (value, ty, checkType))
isTyped (name, value) = Left (name, value)

-- |
-- Map a function over type annotations appearing inside a value
--
overTypes :: (Type -> Type) -> Expr -> Expr
overTypes f = let (_, f', _) = everywhereOnValues id g id in f'
  where
  g :: Expr -> Expr
  g (TypedValue checkTy val t) = TypedValue checkTy val (f t)
  g (TypeClassDictionary c sco) = TypeClassDictionary (mapConstraintArgs (map f) c) sco
  g other = other

-- | Check the kind of a type, failing if it is not of kind *.
checkTypeKind ::
  (MonadError MultipleErrors m) =>
  Type ->
  Kind ->
  m ()
checkTypeKind ty kind = guardWith (errorMessage (ExpectedType ty kind)) $ kind == Star

-- | Remove any ForAlls and ConstrainedType constructors in a type by introducing new unknowns
-- or TypeClassDictionary values.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
instantiatePolyTypeWithUnknowns ::
  (MonadState CheckState m, MonadError MultipleErrors m) =>
  Expr ->
  Type ->
  m (Expr, Type)
instantiatePolyTypeWithUnknowns val (ForAll ident ty _) = do
  ty' <- replaceVarWithUnknown ident ty
  instantiatePolyTypeWithUnknowns val ty'
instantiatePolyTypeWithUnknowns val (ConstrainedType constraints ty) = do
   dicts <- getTypeClassDictionaries
   instantiatePolyTypeWithUnknowns (foldl App val (map (flip TypeClassDictionary dicts) constraints)) ty
instantiatePolyTypeWithUnknowns val ty = return (val, ty)

-- | Infer a type for a value, rethrowing any error to provide a more useful error message
infer ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  Expr ->
  m Expr
infer val = rethrow (addHint (ErrorInferringType val)) $ infer' val

-- | Infer a type for a value
infer' ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  Expr ->
  m Expr
infer' v@(Literal (NumericLiteral (Left _))) = return $ TypedValue True v tyInt
infer' v@(Literal (NumericLiteral (Right _))) = return $ TypedValue True v tyNumber
infer' v@(Literal (StringLiteral _)) = return $ TypedValue True v tyString
infer' v@(Literal (CharLiteral _)) = return $ TypedValue True v tyChar
infer' v@(Literal (BooleanLiteral _)) = return $ TypedValue True v tyBoolean
infer' (Literal (ArrayLiteral vals)) = do
  ts <- traverse infer vals
  els <- freshType
  forM_ ts $ \(TypedValue _ _ t) -> unifyTypes els t
  return $ TypedValue True (Literal (ArrayLiteral ts)) (TypeApp tyArray els)
infer' (Literal (ObjectLiteral ps)) = do
  ensureNoDuplicateProperties ps
  ts <- traverse (infer . snd) ps
  let fields = zipWith (\name (TypedValue _ _ t) -> (name, t)) (map fst ps) ts
      ty = TypeApp tyRecord $ rowFromList (fields, REmpty)
  return $ TypedValue True (Literal (ObjectLiteral (zip (map fst ps) ts))) ty
infer' (ObjectUpdate o ps) = do
  ensureNoDuplicateProperties ps
  row <- freshType
  newVals <- zipWith (\(name, _) t -> (name, t)) ps <$> traverse (infer . snd) ps
  let newTys = map (\(name, TypedValue _ _ ty) -> (name, ty)) newVals
  oldTys <- zip (map fst ps) <$> replicateM (length ps) freshType
  let oldTy = TypeApp tyRecord $ rowFromList (oldTys, row)
  o' <- TypedValue True <$> check o oldTy <*> pure oldTy
  return $ TypedValue True (ObjectUpdate o' newVals) $ TypeApp tyRecord $ rowFromList (newTys, row)
infer' (Accessor prop val) = rethrow (addHint (ErrorCheckingAccessor val prop)) $ do
  field <- freshType
  rest <- freshType
  typed <- check val (TypeApp tyRecord (RCons prop field rest))
  return $ TypedValue True (Accessor prop typed) field
infer' (Abs (Left arg) ret) = do
  ty <- freshType
  Just moduleName <- checkCurrentModule <$> get
  withBindingGroupVisible $ bindLocalVariables moduleName [(arg, ty, Defined)] $ do
    body@(TypedValue _ _ bodyTy) <- infer' ret
    return $ TypedValue True (Abs (Left arg) body) $ function ty bodyTy
infer' (Abs (Right _) _) = internalError "Binder was not desugared"
infer' (App f arg) = do
  f'@(TypedValue _ _ ft) <- infer f
  (ret, app) <- checkFunctionApplication f' ft arg Nothing
  return $ TypedValue True app ret
infer' (Var var) = do
  Just moduleName <- checkCurrentModule <$> get
  checkVisibility moduleName var
  ty <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards <=< lookupVariable moduleName $ var
  case ty of
    ConstrainedType constraints ty' -> do
      dicts <- getTypeClassDictionaries
      return $ TypedValue True (foldl App (Var var) (map (flip TypeClassDictionary dicts) constraints)) ty'
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
infer' (SuperClassDictionary className tys) = do
  dicts <- getTypeClassDictionaries
  return $ TypeClassDictionary (Constraint className tys Nothing) dicts
infer' (TypedValue checkType val ty) = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  val' <- if checkType then withScopedTypeVars moduleName args (check val ty') else return val
  return $ TypedValue True val' ty'
infer' (Hole name) = do
  ty <- freshType
  env <- M.toList . names <$> getEnv
  Just moduleName <- checkCurrentModule <$> get
  let ctx = [ (ident, ty') | ((mn, ident@Ident{}), (ty', _, Defined)) <- env, mn == moduleName ]
  tell . errorMessage $ HoleInferredType name ty ctx
  return $ TypedValue True (Hole name) ty
infer' (PositionedValue pos c val) = warnAndRethrowWithPosition pos $ do
  TypedValue t v ty <- infer' val
  return $ TypedValue t (PositionedValue pos c v) ty
infer' v = internalError $ "Invalid argument to infer: " ++ show v

inferLetBinding ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  [Declaration] ->
  [Declaration] ->
  Expr ->
  (Expr -> m Expr) ->
  m ([Declaration], Expr)
inferLetBinding seen [] ret j = (,) seen <$> withBindingGroupVisible (j ret)
inferLetBinding seen (ValueDeclaration ident nameKind [] (Right (tv@(TypedValue checkType val ty))) : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  let dict = M.singleton (moduleName, ident) (ty, nameKind, Undefined)
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  TypedValue _ val' ty'' <- if checkType then withScopedTypeVars moduleName args (bindNames dict (check val ty')) else return tv
  bindNames (M.singleton (moduleName, ident) (ty'', nameKind, Defined)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] (Right (TypedValue checkType val' ty''))]) rest ret j
inferLetBinding seen (ValueDeclaration ident nameKind [] (Right val) : rest) ret j = do
  valTy <- freshType
  Just moduleName <- checkCurrentModule <$> get
  let dict = M.singleton (moduleName, ident) (valTy, nameKind, Undefined)
  TypedValue _ val' valTy' <- bindNames dict $ infer val
  unifyTypes valTy valTy'
  bindNames (M.singleton (moduleName, ident) (valTy', nameKind, Defined)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] (Right val')]) rest ret j
inferLetBinding seen (BindingGroupDeclaration ds : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  (untyped, typed, dict, untypedDict) <- typeDictionaryForBindingGroup moduleName (map (\(i, _, v) -> (i, v)) ds)
  ds1' <- parU typed $ \e -> checkTypedBindingGroupElement moduleName e dict
  ds2' <- forM untyped $ \e -> typeForBindingGroupElement e dict untypedDict
  let ds' = [(ident, Private, val') | (ident, (val', _)) <- ds1' ++ ds2']
  bindNames dict $ do
    makeBindingGroupVisible
    inferLetBinding (seen ++ [BindingGroupDeclaration ds']) rest ret j
inferLetBinding seen (PositionedDeclaration pos com d : ds) ret j = warnAndRethrowWithPosition pos $ do
  (d' : ds', val') <- inferLetBinding seen (d : ds) ret j
  return (PositionedDeclaration pos com d' : ds', val')
inferLetBinding _ _ _ _ = internalError "Invalid argument to inferLetBinding"

-- | Infer the types of variables brought into scope by a binder
inferBinder :: forall m.
  (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  Type ->
  Binder ->
  m (M.Map Ident Type)
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
  inferRowProperties :: Type -> Type -> [(String, Binder)] -> m (M.Map Ident Type)
  inferRowProperties nrow row [] = unifyTypes nrow row >> return M.empty
  inferRowProperties nrow row ((name, binder):binders) = do
    propTy <- freshType
    m1 <- inferBinder propTy binder
    m2 <- inferRowProperties nrow (RCons name propTy row) binders
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
  warnAndRethrowWithPosition pos $ inferBinder val binder
-- TODO: When adding support for polymorphic types, check subsumption here,
-- change the definition of `binderRequiresMonotype`,
-- and use `kindOfWithScopedVars`.
inferBinder val (TypedBinder ty binder) = do
  kind <- kindOf ty
  checkTypeKind ty kind
  ty1 <- replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  unifyTypes val ty1
  inferBinder val binder
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
binderRequiresMonotype _ = True

-- | Instantiate polytypes only when necessitated by a binder.
instantiateForBinders ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  [Expr] ->
  [CaseAlternative] ->
  m ([Expr], [Type])
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
checkBinders ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  [Type] ->
  Type ->
  [CaseAlternative] ->
  m [CaseAlternative]
checkBinders _ _ [] = return []
checkBinders nvals ret (CaseAlternative binders result : bs) = do
  guardWith (errorMessage $ OverlappingArgNames Nothing) $
    let ns = concatMap binderNames binders in length (nub ns) == length ns
  Just moduleName <- checkCurrentModule <$> get
  m1 <- M.unions <$> zipWithM inferBinder nvals binders
  r <- bindLocalVariables moduleName [ (name, ty, Defined) | (name, ty) <- M.toList m1 ] $
    CaseAlternative binders <$>
      case result of
        Left gs -> do
          gs' <- forM gs $ \(grd, val) -> do
            grd' <- rethrow (addHint ErrorCheckingGuard) $ check grd tyBoolean
            val' <- TypedValue True <$> check val ret <*> pure ret
            return (grd', val')
          return $ Left gs'
        Right val -> do
          val' <- TypedValue True <$> check val ret <*> pure ret
          return $ Right val'
  rs <- checkBinders nvals ret bs
  return $ r : rs

-- |
-- Check the type of a value, rethrowing errors to provide a better error message
--
check ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  Expr ->
  Type ->
  m Expr
check val ty = rethrow (addHint (ErrorCheckingType val ty)) $ check' val ty

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
check' val t@(ConstrainedType constraints ty) = do
  dictNames <- forM constraints $ \(Constraint (Qualified _ (ProperName className)) _ _) ->
    freshIdent ("dict" ++ className)
  dicts <- join <$> zipWithM (newDictionaries []) (map (Qualified Nothing) dictNames) constraints
  val' <- withBindingGroupVisible $ withTypeClassDictionaries dicts $ check val ty
  return $ TypedValue True (foldr (Abs . Left) val' dictNames) t
  where
  -- | Add a dictionary for the constraint to the scope, and dictionaries
  -- for all implied superclass instances.
  newDictionaries
    :: [(Qualified (ProperName 'ClassName), Integer)]
    -> Qualified Ident
    -> Constraint
    -> m [TypeClassDictionaryInScope]
  newDictionaries path name (Constraint className instanceTy _) = do
    tcs <- gets (typeClasses . checkEnv)
    let (args, _, superclasses) = fromMaybe (internalError "newDictionaries: type class lookup failed") $ M.lookup className tcs
    supDicts <- join <$> zipWithM (\(Constraint supName supArgs _) index ->
                                      newDictionaries ((supName, index) : path)
                                                      name
                                                      (Constraint supName (instantiateSuperclass (map fst args) supArgs instanceTy) Nothing)
                                  ) superclasses [0..]
    return (TypeClassDictionaryInScope name path className instanceTy Nothing : supDicts)

  instantiateSuperclass :: [String] -> [Type] -> [Type] -> [Type]
  instantiateSuperclass args supArgs tys = map (replaceAllTypeVars (zip args tys)) supArgs
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
check' (Abs (Left arg) ret) ty@(TypeApp (TypeApp t argTy) retTy) = do
  unifyTypes t tyFunction
  Just moduleName <- checkCurrentModule <$> get
  ret' <- withBindingGroupVisible $ bindLocalVariables moduleName [(arg, argTy, Defined)] $ check ret retTy
  return $ TypedValue True (Abs (Left arg) ret') ty
check' (Abs (Right _) _) _ = internalError "Binder was not desugared"
check' (App f arg) ret = do
  f'@(TypedValue _ _ ft) <- infer f
  (_, app) <- checkFunctionApplication f' ft arg (Just ret)
  return $ TypedValue True app ret
check' v@(Var var) ty = do
  Just moduleName <- checkCurrentModule <$> get
  checkVisibility moduleName var
  repl <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable moduleName $ var
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  v' <- subsumes (Just v) repl ty'
  case v' of
    Nothing -> internalError "check: unable to check the subsumes relation."
    Just v'' -> return $ TypedValue True v'' ty'
check' (SuperClassDictionary className tys) _ = do
  {-
  -- Here, we replace a placeholder for a superclass dictionary with a regular
  -- TypeClassDictionary placeholder. The reason we do this is that it is necessary to have the
  -- correct super instance dictionaries in scope, and these are not available when the type class
  -- declaration gets desugared.
  -}
  dicts <- getTypeClassDictionaries
  return $ TypeClassDictionary (Constraint className tys Nothing) dicts
check' (TypedValue checkType val ty1) ty2 = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- kindOfWithScopedVars ty1
  checkTypeKind ty1 kind
  ty1' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty1
  ty2' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty2
  val' <- subsumes (Just val) ty1' ty2'
  case val' of
    Nothing -> internalError "check: unable to check the subsumes relation."
    Just _ -> do
      val''' <- if checkType then withScopedTypeVars moduleName args (check val ty2') else return val
      return $ TypedValue checkType val''' ty2'
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
      (removedProps, remainingProps) = partition (\(p, _) -> p `elem` map fst ps) propsToCheck
  us <- zip (map fst removedProps) <$> replicateM (length ps) freshType
  obj' <- check obj (TypeApp tyRecord (rowFromList (us ++ remainingProps, rest)))
  ps' <- checkProperties e ps row True
  return $ TypedValue True (ObjectUpdate obj' ps') t
check' (Accessor prop val) ty = rethrow (addHint (ErrorCheckingAccessor val prop)) $ do
  rest <- freshType
  val' <- check val (TypeApp tyRecord (RCons prop ty rest))
  return $ TypedValue True (Accessor prop val') ty
check' v@(Constructor c) ty = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty1, _) -> do
      repl <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
      mv <- subsumes (Just v) repl ty
      case mv of
        Nothing -> internalError "check: unable to check the subsumes relation."
        Just v' -> return $ TypedValue True v' ty
check' (Let ds val) ty = do
  (ds', val') <- inferLetBinding [] ds val (`check` ty)
  return $ TypedValue True (Let ds' val') ty
check' val kt@(KindedType ty kind) = do
  checkTypeKind ty kind
  val' <- check' val ty
  return $ TypedValue True val' kt
check' (PositionedValue pos c val) ty = warnAndRethrowWithPosition pos $ do
  TypedValue t v ty' <- check' val ty
  return $ TypedValue t (PositionedValue pos c v) ty'
check' val ty = do
  TypedValue _ val' ty' <- infer val
  mt <- subsumes (Just val') ty' ty
  case mt of
    Nothing -> internalError "check: unable to check the subsumes relation."
    Just v' -> return $ TypedValue True v' ty

-- |
-- Check the type of a collection of named record fields
--
-- The @lax@ parameter controls whether or not every record member has to be provided. For object updates, this is not the case.
--
checkProperties ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  Expr ->
  [(String, Expr)] ->
  Type ->
  Bool ->
  m [(String, Expr)]
checkProperties expr ps row lax = let (ts, r') = rowToList row in go ps ts r' where
  go [] [] REmpty = return []
  go [] [] u@(TUnknown _)
    | lax = return []
    | otherwise = do unifyTypes u REmpty
                     return []
  go [] [] Skolem{} | lax = return []
  go [] ((p, _): _) _ | lax = return []
                      | otherwise = throwError . errorMessage $ PropertyIsMissing p
  go ((p,_):_) [] REmpty = throwError . errorMessage $ AdditionalProperty p
  go ((p,v):ps') ts r =
    case lookup p ts of
      Nothing -> do
        v'@(TypedValue _ _ ty) <- infer v
        rest <- freshType
        unifyTypes r (RCons p ty rest)
        ps'' <- go ps' ts rest
        return $ (p, v') : ps''
      Just ty -> do
        v' <- check v ty
        ps'' <- go ps' (delete (p, ty) ts) r
        return $ (p, v') : ps''
  go _ _ _ = throwError . errorMessage $ ExprDoesNotHaveType expr (TypeApp tyRecord row)

-- | Check the type of a function application, rethrowing errors to provide a better error message
checkFunctionApplication ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  Expr ->
  Type ->
  Expr ->
  Maybe Type ->
  m (Type, Expr)
checkFunctionApplication fn fnTy arg ret = rethrow (addHint (ErrorInApplication fn fnTy arg)) $ do
  subst <- gets checkSubstitution
  checkFunctionApplication' fn (substituteType subst fnTy) arg (substituteType subst <$> ret)

-- | Check the type of a function application
checkFunctionApplication' ::
  (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  Expr ->
  Type ->
  Expr ->
  Maybe Type ->
  m (Type, Expr)
checkFunctionApplication' fn (TypeApp (TypeApp tyFunction' argTy) retTy) arg ret = do
  unifyTypes tyFunction' tyFunction
  arg' <- check arg argTy
  case ret of
    Nothing -> return (retTy, App fn arg')
    Just ret' -> do
      Just app' <- subsumes (Just (App fn arg')) retTy ret'
      return (retTy, app')
checkFunctionApplication' fn (ForAll ident ty _) arg ret = do
  replaced <- replaceVarWithUnknown ident ty
  checkFunctionApplication fn replaced arg ret
checkFunctionApplication' fn u@(TUnknown _) arg ret = do
  arg' <- do
    TypedValue _ arg' t <- infer arg
    (arg'', t') <- instantiatePolyTypeWithUnknowns arg' t
    return $ TypedValue True arg'' t'
  let ty = (\(TypedValue _ _ t) -> t) arg'
  ret' <- maybe freshType return ret
  unifyTypes u (function ty ret')
  return (ret', App fn arg')
checkFunctionApplication' fn (KindedType ty _) arg ret =
  checkFunctionApplication fn ty arg ret
checkFunctionApplication' fn (ConstrainedType constraints fnTy) arg ret = do
  dicts <- getTypeClassDictionaries
  checkFunctionApplication' (foldl App fn (map (flip TypeClassDictionary dicts) constraints)) fnTy arg ret
checkFunctionApplication' fn fnTy dict@TypeClassDictionary{} _ =
  return (fnTy, App fn dict)
checkFunctionApplication' _ fnTy arg _ = throwError . errorMessage $ CannotApplyFunction fnTy arg

-- |
-- Ensure a set of property names and value does not contain duplicate labels
--
ensureNoDuplicateProperties :: (MonadError MultipleErrors m) => [(String, Expr)] -> m ()
ensureNoDuplicateProperties ps =
  let ls = map fst ps in
  case ls \\ nub ls of
    l : _ -> throwError . errorMessage $ DuplicateLabel l Nothing
    _ -> return ()
