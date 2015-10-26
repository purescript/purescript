-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Types
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the type checker
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.TypeChecker.Types (
    typesOf
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

import Data.Either (lefts, rights)
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.State
import Control.Monad.Unify
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (tell)

import Language.PureScript.Crash
import Language.PureScript.AST
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

-- |
-- Infer the types of multiple mutually-recursive values, and return elaborated values including
-- type class dictionaries and type annotations.
--
typesOf :: ModuleName -> [(Ident, Expr)] -> Check [(Ident, (Expr, Type))]
typesOf moduleName vals = do
  tys <- fmap tidyUp . liftUnifyWarnings replace $ do
    (untyped, typed, dict, untypedDict) <- typeDictionaryForBindingGroup moduleName vals
    ds1 <- parU typed $ \e -> checkTypedBindingGroupElement moduleName e dict
    ds2 <- forM untyped $ \e -> typeForBindingGroupElement True e dict untypedDict
    return $ ds1 ++ ds2

  forM tys $ \(ident, (val, ty)) -> do
    -- Replace type class dictionary placeholders with actual dictionaries
    val' <- replaceTypeClassDictionaries moduleName val
    -- Check skolem variables did not escape their scope
    skolemEscapeCheck val'
    -- Check rows do not contain duplicate labels
    checkDuplicateLabels val'
    return (ident, (val', varIfUnknown ty))
  where
  -- Apply the substitution that was returned from runUnify to both types and (type-annotated) values
  tidyUp (ts, sub) = map (\(i, (val, ty)) -> (i, (overTypes (sub $?) val, sub $? ty))) ts
  -- Replace all the wildcards types with their inferred types
  replace sub (ErrorMessage hints (WildcardInferredType ty)) = ErrorMessage hints $ WildcardInferredType (sub $? ty)
  replace sub (ErrorMessage hints (MissingTypeDeclaration name ty)) = ErrorMessage hints $ MissingTypeDeclaration name (varIfUnknown (sub $? ty))
  replace _ em = em

type TypeData = M.Map (ModuleName, Ident) (Type, NameKind, NameVisibility)

type UntypedData = [(Ident, Type)]

typeDictionaryForBindingGroup :: ModuleName -> [(Ident, Expr)] -> UnifyT Type Check ([(Ident, Expr)], [(Ident, (Expr, Type, Bool))], TypeData, UntypedData)
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
  untypedNames <- replicateM (length untyped) fresh

  let
    -- Make a map of names to the unification variables of untyped declarations
    untypedDict = zip (map fst untyped) untypedNames
    -- Create the dictionary of all name/type pairs, which will be added to the environment during type checking
    dict = M.fromList (map (\(ident, ty) -> ((moduleName, ident), (ty, Private, Undefined))) $ typedDict ++ untypedDict)
  return (untyped, typed, dict, untypedDict)

checkTypedBindingGroupElement :: ModuleName -> (Ident, (Expr, Type, Bool)) -> TypeData ->  UnifyT Type Check (Ident, (Expr, Type))
checkTypedBindingGroupElement mn (ident, (val', ty, checkType)) dict = do
  -- Replace type wildcards
  ty' <- replaceTypeWildcards ty
  -- Kind check
  (kind, args) <- liftCheck $ kindOfWithScopedVars ty
  checkTypeKind ty kind
  -- Check the type with the new names in scope
  ty'' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty'
  val'' <- if checkType
           then withScopedTypeVars mn args $ bindNames dict $ TypedValue True <$> check val' ty'' <*> pure ty''
           else return (TypedValue False val' ty'')
  return (ident, (val'', ty''))

typeForBindingGroupElement :: Bool -> (Ident, Expr) -> TypeData -> UntypedData -> UnifyT Type Check (Ident, (Expr, Type))
typeForBindingGroupElement warn (ident, val) dict untypedDict = do
  -- Infer the type with the new names in scope
  TypedValue _ val' ty <- bindNames dict $ infer val
  ty =?= fromMaybe (internalError "name not found in dictionary") (lookup ident untypedDict)
  when warn . tell . errorMessage $ MissingTypeDeclaration ident ty
  return (ident, (TypedValue True val' ty, ty))

-- |
-- Check if a value contains a type annotation
--
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
  g (TypeClassDictionary (nm, tys) sco) = TypeClassDictionary (nm, map f tys) sco
  g other = other

-- |
-- Replace type class dictionary placeholders with inferred type class dictionaries
--
replaceTypeClassDictionaries :: ModuleName -> Expr -> Check Expr
replaceTypeClassDictionaries mn =
  let (_, f, _) = everywhereOnValuesTopDownM return go return
  in f
  where
  go (TypeClassDictionary constraint dicts) = entails mn dicts constraint
  go other = return other

-- |
-- Check the kind of a type, failing if it is not of kind *.
--
checkTypeKind :: Type -> Kind -> UnifyT t Check ()
checkTypeKind ty kind = guardWith (errorMessage (ExpectedType ty kind)) $ kind == Star

-- |
-- Remove any ForAlls and ConstrainedType constructors in a type by introducing new unknowns
-- or TypeClassDictionary values.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
--
instantiatePolyTypeWithUnknowns :: Expr -> Type -> UnifyT Type Check (Expr, Type)
instantiatePolyTypeWithUnknowns val (ForAll ident ty _) = do
  ty' <- replaceVarWithUnknown ident ty
  instantiatePolyTypeWithUnknowns val ty'
instantiatePolyTypeWithUnknowns val (ConstrainedType constraints ty) = do
   dicts <- getTypeClassDictionaries
   (_, ty') <- instantiatePolyTypeWithUnknowns (internalError "Types under a constraint cannot themselves be constrained") ty
   return (foldl App val (map (flip TypeClassDictionary dicts) constraints), ty')
instantiatePolyTypeWithUnknowns val ty = return (val, ty)

-- |
-- Infer a type for a value, rethrowing any error to provide a more useful error message
--
infer :: Expr -> UnifyT Type Check Expr
infer val = rethrow (addHint (ErrorInferringType val)) $ infer' val

-- |
-- Infer a type for a value
--
infer' :: Expr -> UnifyT Type Check Expr
infer' v@(NumericLiteral (Left _)) = return $ TypedValue True v tyInt
infer' v@(NumericLiteral (Right _)) = return $ TypedValue True v tyNumber
infer' v@(StringLiteral _) = return $ TypedValue True v tyString
infer' v@(CharLiteral _) = return $ TypedValue True v tyChar
infer' v@(BooleanLiteral _) = return $ TypedValue True v tyBoolean
infer' (ArrayLiteral vals) = do
  ts <- mapM infer vals
  els <- fresh
  forM_ ts $ \(TypedValue _ _ t) -> els =?= t
  return $ TypedValue True (ArrayLiteral ts) (TypeApp tyArray els)
infer' (ObjectLiteral ps) = do
  ensureNoDuplicateProperties ps
  ts <- mapM (infer . snd) ps
  let fields = zipWith (\name (TypedValue _ _ t) -> (name, t)) (map fst ps) ts
      ty = TypeApp tyObject $ rowFromList (fields, REmpty)
  return $ TypedValue True (ObjectLiteral (zip (map fst ps) ts)) ty
infer' (ObjectUpdate o ps) = do
  ensureNoDuplicateProperties ps
  row <- fresh
  newVals <- zipWith (\(name, _) t -> (name, t)) ps <$> mapM (infer . snd) ps
  let newTys = map (\(name, TypedValue _ _ ty) -> (name, ty)) newVals
  oldTys <- zip (map fst ps) <$> replicateM (length ps) fresh
  let oldTy = TypeApp tyObject $ rowFromList (oldTys, row)
  o' <- TypedValue True <$> check o oldTy <*> pure oldTy
  return $ TypedValue True (ObjectUpdate o' newVals) $ TypeApp tyObject $ rowFromList (newTys, row)
infer' (Accessor prop val) = rethrow (addHint (ErrorCheckingAccessor val prop)) $ do
  field <- fresh
  rest <- fresh
  typed <- check val (TypeApp tyObject (RCons prop field rest))
  return $ TypedValue True (Accessor prop typed) field
infer' (Abs (Left arg) ret) = do
  ty <- fresh
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
    Nothing -> throwError . errorMessage $ UnknownDataConstructor c Nothing
    Just (_, _, ty, _) -> do (v', ty') <- sndM (introduceSkolemScope <=< replaceAllTypeSynonyms) <=< instantiatePolyTypeWithUnknowns v $ ty
                             return $ TypedValue True v' ty'
infer' (Case vals binders) = do
  (vals', ts) <- instantiateForBinders vals binders
  ret <- fresh
  binders' <- checkBinders ts ret binders
  return $ TypedValue True (Case vals' binders') ret
infer' (IfThenElse cond th el) = do
  cond' <- check cond tyBoolean
  v2@(TypedValue _ _ t2) <- infer th
  v3@(TypedValue _ _ t3) <- infer el
  (v2', v3', t) <- meet v2 v3 t2 t3
  return $ TypedValue True (IfThenElse cond' v2' v3') t
infer' (Let ds val) = do
  (ds', val'@(TypedValue _ _ valTy)) <- inferLetBinding [] ds val infer
  return $ TypedValue True (Let ds' val') valTy
infer' (SuperClassDictionary className tys) = do
  dicts <- getTypeClassDictionaries
  return $ TypeClassDictionary (className, tys) dicts
infer' (TypedValue checkType val ty) = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- liftCheck $ kindOfWithScopedVars ty
  checkTypeKind ty kind
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  val' <- if checkType then withScopedTypeVars moduleName args (check val ty') else return val
  return $ TypedValue True val' ty'
infer' (PositionedValue pos _ val) = warnAndRethrowWithPosition pos $ infer' val
infer' _ = internalError "Invalid argument to infer"

inferLetBinding :: [Declaration] -> [Declaration] -> Expr -> (Expr -> UnifyT Type Check Expr) -> UnifyT Type Check ([Declaration], Expr)
inferLetBinding seen [] ret j = (,) seen <$> withBindingGroupVisible (j ret)
inferLetBinding seen (ValueDeclaration ident nameKind [] (Right (tv@(TypedValue checkType val ty))) : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- liftCheck $ kindOfWithScopedVars ty
  checkTypeKind ty kind
  let dict = M.singleton (moduleName, ident) (ty, nameKind, Undefined)
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  TypedValue _ val' ty'' <- if checkType then withScopedTypeVars moduleName args (bindNames dict (check val ty')) else return tv
  bindNames (M.singleton (moduleName, ident) (ty'', nameKind, Defined)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] (Right (TypedValue checkType val' ty''))]) rest ret j
inferLetBinding seen (ValueDeclaration ident nameKind [] (Right val) : rest) ret j = do
  valTy <- fresh
  Just moduleName <- checkCurrentModule <$> get
  let dict = M.singleton (moduleName, ident) (valTy, nameKind, Undefined)
  TypedValue _ val' valTy' <- bindNames dict $ infer val
  valTy =?= valTy'
  bindNames (M.singleton (moduleName, ident) (valTy', nameKind, Defined)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] (Right val')]) rest ret j
inferLetBinding seen (BindingGroupDeclaration ds : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  (untyped, typed, dict, untypedDict) <- typeDictionaryForBindingGroup moduleName (map (\(i, _, v) -> (i, v)) ds)
  ds1' <- parU typed $ \e -> checkTypedBindingGroupElement moduleName e dict
  ds2' <- forM untyped $ \e -> typeForBindingGroupElement False e dict untypedDict
  let ds' = [(ident, Private, val') | (ident, (val', _)) <- ds1' ++ ds2']
  bindNames dict $ do
    makeBindingGroupVisible
    inferLetBinding (seen ++ [BindingGroupDeclaration ds']) rest ret j
inferLetBinding seen (PositionedDeclaration pos com d : ds) ret j = warnAndRethrowWithPosition pos $ do
  (d' : ds', val') <- inferLetBinding seen (d : ds) ret j
  return (PositionedDeclaration pos com d' : ds', val')
inferLetBinding _ _ _ _ = internalError "Invalid argument to inferLetBinding"

-- |
-- Infer the types of variables brought into scope by a binder
--
inferBinder :: Type -> Binder -> UnifyT Type Check (M.Map Ident Type)
inferBinder _ NullBinder = return M.empty
inferBinder val (StringBinder _) = val =?= tyString >> return M.empty
inferBinder val (CharBinder _) = val =?= tyChar >> return M.empty
inferBinder val (NumberBinder (Left _)) = val =?= tyInt >> return M.empty
inferBinder val (NumberBinder (Right _)) = val =?= tyNumber >> return M.empty
inferBinder val (BooleanBinder _) = val =?= tyBoolean >> return M.empty
inferBinder val (VarBinder name) = return $ M.singleton name val
inferBinder val (ConstructorBinder ctor binders) = do
  env <- getEnv
  case M.lookup ctor (dataConstructors env) of
    Just (_, _, ty, _) -> do
      (_, fn) <- instantiatePolyTypeWithUnknowns (internalError "Data constructor types cannot contain constraints") ty
      fn' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ fn
      let (args, ret) = peelArgs fn'
      unless (length args == length binders) . throwError . errorMessage $ IncorrectConstructorArity ctor
      ret =?= val
      M.unions <$> zipWithM inferBinder (reverse args) binders
    _ -> throwError . errorMessage $ UnknownDataConstructor ctor Nothing
  where
  peelArgs :: Type -> ([Type], Type)
  peelArgs = go []
    where
    go args (TypeApp (TypeApp fn arg) ret) | fn == tyFunction = go (arg : args) ret
    go args ret = (args, ret)
inferBinder val (ObjectBinder props) = do
  row <- fresh
  rest <- fresh
  m1 <- inferRowProperties row rest props
  val =?= TypeApp tyObject row
  return m1
  where
  inferRowProperties :: Type -> Type -> [(String, Binder)] -> UnifyT Type Check (M.Map Ident Type)
  inferRowProperties nrow row [] = nrow =?= row >> return M.empty
  inferRowProperties nrow row ((name, binder):binders) = do
    propTy <- fresh
    m1 <- inferBinder propTy binder
    m2 <- inferRowProperties nrow (RCons name propTy row) binders
    return $ m1 `M.union` m2
inferBinder val (ArrayBinder binders) = do
  el <- fresh
  m1 <- M.unions <$> mapM (inferBinder el) binders
  val =?= TypeApp tyArray el
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
  ty1 <- replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  kind <- liftCheck $ kindOf ty1
  checkTypeKind ty1 kind
  val =?= ty1
  inferBinder val binder

-- | Returns true if a binder requires its argument type to be a monotype.
-- | If this is the case, we need to instantiate any polymorphic types before checking binders.
binderRequiresMonotype :: Binder -> Bool
binderRequiresMonotype NullBinder = False
binderRequiresMonotype (VarBinder _) = False
binderRequiresMonotype (NamedBinder _ b) = binderRequiresMonotype b
binderRequiresMonotype (PositionedBinder _ _ b) = binderRequiresMonotype b
binderRequiresMonotype _ = True

-- | Instantiate polytypes only when necessitated by a binder.
instantiateForBinders :: [Expr] -> [CaseAlternative] -> UnifyT Type Check ([Expr], [Type])
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
checkBinders :: [Type] -> Type -> [CaseAlternative] -> UnifyT Type Check [CaseAlternative]
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
            grd' <- check grd tyBoolean
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
check :: Expr -> Type -> UnifyT Type Check Expr
check val ty = rethrow (addHint (ErrorCheckingType val ty)) $ check' val ty

-- |
-- Check the type of a value
--
check' :: Expr -> Type -> UnifyT Type Check Expr
check' val (ForAll ident ty _) = do
  scope <- newSkolemScope
  sko <- newSkolemConstant
  let sk = skolemize ident sko scope ty
  let skVal = skolemizeTypesInValue ident sko scope val
  val' <- check skVal sk
  return $ TypedValue True val' (ForAll ident ty (Just scope))
check' val t@(ConstrainedType constraints ty) = do
  dictNames <- forM constraints $ \(Qualified _ (ProperName className), _) -> do
    n <- liftCheck freshDictionaryName
    return $ Ident $ "__dict_" ++ className ++ "_" ++ show n
  dicts <- join <$> liftCheck (zipWithM (newDictionaries []) (map (Qualified Nothing) dictNames) constraints)
  val' <- withBindingGroupVisible $ withTypeClassDictionaries dicts $ check val ty
  return $ TypedValue True (foldr (Abs . Left) val' dictNames) t
  where
  -- | Add a dictionary for the constraint to the scope, and dictionaries
  -- for all implies superclass instances.
  newDictionaries :: [(Qualified ProperName, Integer)] -> Qualified Ident -> (Qualified ProperName, [Type]) -> Check [TypeClassDictionaryInScope]
  newDictionaries path name (className, instanceTy) = do
    tcs <- gets (typeClasses . checkEnv)
    let (args, _, superclasses) = fromMaybe (internalError "newDictionaries: type class lookup failed") $ M.lookup className tcs
    supDicts <- join <$> zipWithM (\(supName, supArgs) index ->
                                      newDictionaries ((supName, index) : path)
                                                      name
                                                      (supName, instantiateSuperclass (map fst args) supArgs instanceTy)
                                  ) superclasses [0..]
    return (TypeClassDictionaryInScope name path className instanceTy Nothing : supDicts)

  instantiateSuperclass :: [String] -> [Type] -> [Type] -> [Type]
  instantiateSuperclass args supArgs tys = map (replaceAllTypeVars (zip args tys)) supArgs
check' val u@(TUnknown _) = do
  val'@(TypedValue _ _ ty) <- infer val
  -- Don't unify an unknown with an inferred polytype
  (val'', ty') <- instantiatePolyTypeWithUnknowns val' ty
  ty' =?= u
  return $ TypedValue True val'' ty'
check' v@(NumericLiteral (Left _)) t | t == tyInt =
  return $ TypedValue True v t
check' v@(NumericLiteral (Right _)) t | t == tyNumber =
  return $ TypedValue True v t
check' v@(StringLiteral _) t | t == tyString =
  return $ TypedValue True v t
check' v@(CharLiteral _) t | t == tyChar =
  return $ TypedValue True v t
check' v@(BooleanLiteral _) t | t == tyBoolean =
  return $ TypedValue True v t
check' (ArrayLiteral vals) t@(TypeApp a ty) = do
  a =?= tyArray
  array <- ArrayLiteral <$> forM vals (`check` ty)
  return $ TypedValue True array t
check' (Abs (Left arg) ret) ty@(TypeApp (TypeApp t argTy) retTy) = do
  t =?= tyFunction
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
  return $ TypeClassDictionary (className, tys) dicts
check' (TypedValue checkType val ty1) ty2 = do
  Just moduleName <- checkCurrentModule <$> get
  (kind, args) <- liftCheck $ kindOfWithScopedVars ty1
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
check' e@(ObjectLiteral ps) t@(TypeApp obj row) | obj == tyObject = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties e ps row False
  return $ TypedValue True (ObjectLiteral ps') t
check' (TypeClassDictionaryConstructorApp name ps) t = do
  ps' <- check' ps t
  return $ TypedValue True (TypeClassDictionaryConstructorApp name ps') t
check' e@(ObjectUpdate obj ps) t@(TypeApp o row) | o == tyObject = do
  ensureNoDuplicateProperties ps
  -- We need to be careful to avoid duplicate labels here.
  -- We check _obj_ against the type _t_ with the types in _ps_ replaced with unknowns.
  let (propsToCheck, rest) = rowToList row
      (removedProps, remainingProps) = partition (\(p, _) -> p `elem` map fst ps) propsToCheck
  us <- zip (map fst removedProps) <$> replicateM (length ps) fresh
  obj' <- check obj (TypeApp tyObject (rowFromList (us ++ remainingProps, rest)))
  ps' <- checkProperties e ps row True
  return $ TypedValue True (ObjectUpdate obj' ps') t
check' (Accessor prop val) ty = rethrow (addHint (ErrorCheckingAccessor val prop)) $ do
  rest <- fresh
  val' <- check val (TypeApp tyObject (RCons prop ty rest))
  return $ TypedValue True (Accessor prop val') ty
check' v@(Constructor c) ty = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage $ UnknownDataConstructor c Nothing
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
check' (PositionedValue pos _ val) ty =
  warnAndRethrowWithPosition pos $ check' val ty
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
checkProperties :: Expr -> [(String, Expr)] -> Type -> Bool -> UnifyT Type Check [(String, Expr)]
checkProperties expr ps row lax = let (ts, r') = rowToList row in go ps ts r' where
  go [] [] REmpty = return []
  go [] [] u@(TUnknown _)
    | lax = return []
    | otherwise = do u =?= REmpty
                     return []
  go [] [] Skolem{} | lax = return []
  go [] ((p, _): _) _ | lax = return []
                      | otherwise = throwError . errorMessage $ PropertyIsMissing p
  go ((p,_):_) [] REmpty = throwError . errorMessage $ AdditionalProperty p
  go ((p,v):ps') ts r =
    case lookup p ts of
      Nothing -> do
        v'@(TypedValue _ _ ty) <- infer v
        rest <- fresh
        r =?= RCons p ty rest
        ps'' <- go ps' ts rest
        return $ (p, v') : ps''
      Just ty -> do
        v' <- check v ty
        ps'' <- go ps' (delete (p, ty) ts) r
        return $ (p, v') : ps''
  go _ _ _ = throwError . errorMessage $ ExprDoesNotHaveType expr (TypeApp tyObject row)

-- |
-- Check the type of a function application, rethrowing errors to provide a better error message
--
checkFunctionApplication :: Expr -> Type -> Expr -> Maybe Type -> UnifyT Type Check (Type, Expr)
checkFunctionApplication fn fnTy arg ret = rethrow (addHint (ErrorInApplication fn fnTy arg)) $ do
  subst <- unifyCurrentSubstitution <$> UnifyT get
  checkFunctionApplication' fn (subst $? fnTy) arg (($?) subst <$> ret)

-- |
-- Check the type of a function application
--
checkFunctionApplication' :: Expr -> Type -> Expr -> Maybe Type -> UnifyT Type Check (Type, Expr)
checkFunctionApplication' fn (TypeApp (TypeApp tyFunction' argTy) retTy) arg ret = do
  tyFunction' =?= tyFunction
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
  ret' <- maybe fresh return ret
  u =?= function ty ret'
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
-- Compute the meet of two types, i.e. the most general type which both types subsume.
-- TODO: handle constrained types
--
meet :: Expr -> Expr -> Type -> Type -> UnifyT Type Check (Expr, Expr, Type)
meet e1 e2 (ForAll ident t1 _) t2 = do
  t1' <- replaceVarWithUnknown ident t1
  meet e1 e2 t1' t2
meet e1 e2 t1 (ForAll ident t2 _) = do
  t2' <- replaceVarWithUnknown ident t2
  meet e1 e2 t1 t2'
meet e1 e2 t1 t2 = do
  t1 =?= t2
  return (e1, e2, t1)

-- |
-- Ensure a set of property names and value does not contain duplicate labels
--
ensureNoDuplicateProperties :: (MonadError MultipleErrors m) => [(String, Expr)] -> m ()
ensureNoDuplicateProperties ps =
  let ls = map fst ps in
  case ls \\ nub ls of
    l : _ -> throwError . errorMessage $ DuplicateLabel l Nothing
    _ -> return ()
