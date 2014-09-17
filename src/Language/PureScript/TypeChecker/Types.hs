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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

    subsumes
      Check a type subsumes another type
-}

import Data.List
import Data.Maybe (maybeToList, isNothing, isJust, fromMaybe)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Monoid

import Language.PureScript.Declarations
import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Kinds
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.Pretty
import Language.PureScript.Environment
import Language.PureScript.Errors
import qualified Language.PureScript.Constants as C

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Unify

import Control.Applicative
import Control.Arrow (Arrow(..))

import qualified Data.Map as M
import qualified Data.HashMap.Strict as H

instance Partial Type where
  unknown = TUnknown
  isUnknown (TUnknown u) = Just u
  isUnknown _ = Nothing
  unknowns = everythingOnTypes (++) go
    where
    go (TUnknown u) = [u]
    go _ = []
  ($?) sub = everywhereOnTypes go
    where
    go t@(TUnknown u) = case H.lookup u (runSubstitution sub) of
                          Nothing -> t
                          Just t' -> t'
    go other = other

instance Unifiable Check Type where
  (=?=) = unifyTypes

-- |
-- Unify two types, updating the current substitution
--
unifyTypes :: Type -> Type -> UnifyT Type Check ()
unifyTypes t1 t2 = rethrow (mkErrorStack ("Error unifying type " ++ prettyPrintType t1 ++ " with type " ++ prettyPrintType t2) Nothing <>) $
  unifyTypes' t1 t2
  where
  unifyTypes' (TUnknown u1) (TUnknown u2) | u1 == u2 = return ()
  unifyTypes' (TUnknown u) t = u =:= t
  unifyTypes' t (TUnknown u) = u =:= t
  unifyTypes' (SaturatedTypeSynonym name args) ty = do
    ty1 <- introduceSkolemScope <=< expandTypeSynonym name $ args
    ty1 `unifyTypes` ty
  unifyTypes' ty s@(SaturatedTypeSynonym _ _) = s `unifyTypes` ty
  unifyTypes' (ForAll ident1 ty1 sc1) (ForAll ident2 ty2 sc2) =
    case (sc1, sc2) of
      (Just sc1', Just sc2') -> do
        sko <- newSkolemConstant
        let sk1 = skolemize ident1 sko sc1' ty1
        let sk2 = skolemize ident2 sko sc2' ty2
        sk1 `unifyTypes` sk2
      _ -> error "Skolemized type variable was not given a scope"
  unifyTypes' (ForAll ident ty1 (Just sc)) ty2 = do
    sko <- newSkolemConstant
    let sk = skolemize ident sko sc ty1
    sk `unifyTypes` ty2
  unifyTypes' ForAll{} _ = throwError . strMsg $ "Skolem variable scope is unspecified"
  unifyTypes' ty f@ForAll{} = f `unifyTypes` ty
  unifyTypes' (TypeVar v1) (TypeVar v2) | v1 == v2 = return ()
  unifyTypes' (TypeConstructor c1) (TypeConstructor c2) =
    guardWith (strMsg ("Cannot unify " ++ show c1 ++ " with " ++ show c2 ++ ".")) (c1 == c2)
  unifyTypes' (TypeApp t3 t4) (TypeApp t5 t6) = do
    t3 `unifyTypes` t5
    t4 `unifyTypes` t6
  unifyTypes' (Skolem _ s1 _) (Skolem _ s2 _) | s1 == s2 = return ()
  unifyTypes' r1@RCons{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@RCons{} = unifyRows r1 r2
  unifyTypes' r1@REmpty r2 = unifyRows r1 r2
  unifyTypes' r1 r2@REmpty = unifyRows r1 r2
  unifyTypes' t@(ConstrainedType _ _) _ = throwError . strMsg $ "Attempted to unify a constrained type " ++ prettyPrintType t ++ " with another type."
  unifyTypes' t3 t4@(ConstrainedType _ _) = unifyTypes' t4 t3
  unifyTypes' t3 t4 = throwError . strMsg $ "Cannot unify " ++ prettyPrintType t3 ++ " with " ++ prettyPrintType t4 ++ "."

-- |
-- Unify two rows, updating the current substitution
--
-- Common labels are first identified, and unified. Remaining labels and types are unified with a
-- trailing row unification variable, if appropriate, otherwise leftover labels result in a unification
-- error.
--
unifyRows :: Type -> Type -> UnifyT Type Check ()
unifyRows r1 r2 =
  let
    (s1, r1') = rowToList r1
    (s2, r2') = rowToList r2
    int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
    sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
    sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
  in do
    forM_ int (uncurry (=?=))
    unifyRows' sd1 r1' sd2 r2'
  where
  unifyRows' :: [(String, Type)] -> Type -> [(String, Type)] -> Type -> UnifyT Type Check ()
  unifyRows' [] (TUnknown u) sd r = u =:= rowFromList (sd, r)
  unifyRows' sd r [] (TUnknown u) = u =:= rowFromList (sd, r)
  unifyRows' ((name, ty):row) r others u@(TUnknown un) = do
    occursCheck un ty
    forM_ row $ \(_, t) -> occursCheck un t
    u' <- fresh
    u =?= RCons name ty u'
    unifyRows' row r others u'
  unifyRows' [] REmpty [] REmpty = return ()
  unifyRows' [] (TypeVar v1) [] (TypeVar v2) | v1 == v2 = return ()
  unifyRows' [] (Skolem _ s1 _) [] (Skolem _ s2 _) | s1 == s2 = return ()
  unifyRows' sd3 r3 sd4 r4 = throwError . strMsg $ "Cannot unify (" ++ prettyPrintRow (rowFromList (sd3, r3)) ++ ") with (" ++ prettyPrintRow (rowFromList (sd4, r4)) ++ ")"

-- |
-- Infer the types of multiple mutually-recursive values, and return elaborated values including
-- type class dictionaries and type annotations.
--
typesOf :: Maybe ModuleName -> ModuleName -> [(Ident, Expr)] -> Check [(Ident, (Expr, Type))]
typesOf mainModuleName moduleName vals = do
  tys <- fmap tidyUp . liftUnify $ do
    (es, dict, untypedDict) <- typeDictionaryForBindingGroup moduleName vals
    forM es $ \e -> do
      triple@(_, (_, ty)) <- typeForBindingGroupElement moduleName e dict untypedDict
      -- If --main is enabled, need to check that `main` has type Eff eff a for some eff, a
      when (Just moduleName == mainModuleName && fst e == Ident C.main) $ do
        [eff, a] <- replicateM 2 fresh
        ty =?= TypeApp (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName "Control", ProperName "Monad", ProperName "Eff"])) (ProperName "Eff"))) eff) a
      return triple

  forM tys $ \(ident, (val, ty)) -> do
    -- Replace type class dictionary placeholders with actual dictionaries
    val' <- replaceTypeClassDictionaries moduleName val
    -- Check skolem variables did not escape their scope
    skolemEscapeCheck val'
    -- Remove type synonyms placeholders, remove duplicate row fields, and replace
    -- top-level unification variables with named type variables.
    let val'' = overTypes (desaturateAllTypeSynonyms . setifyAll) val'
        ty' = varIfUnknown . desaturateAllTypeSynonyms . setifyAll $ ty
    return (ident, (val'', ty'))
  where
  -- Apply the substitution that was returned from runUnify to both types and (type-annotated) values
  tidyUp (ts, sub) = map (\(i, (val, ty)) -> (i, (overTypes (sub $?) val, sub $? ty))) ts

typeDictionaryForBindingGroup :: ModuleName -> [(Ident, Expr)] -> UnifyT Type Check ([(Ident, (Expr, Maybe (Type, Bool)))], M.Map (ModuleName, Ident) (Type, NameKind, NameVisibility), [(Ident, Type)])
typeDictionaryForBindingGroup moduleName vals = do
  let
    -- Map each declaration to a name/value pair, with an optional type, if the declaration is typed
    es = map isTyped vals
    -- Filter the typed and untyped declarations
    typed = filter (isJust . snd . snd) es
    untyped = filter (isNothing . snd . snd) es
    -- Make a map of names to typed declarations
    typedDict = map (\(ident, (_, Just (ty, _))) -> (ident, ty)) typed

  -- Create fresh unification variables for the types of untyped declarations
  untypedNames <- replicateM (length untyped) fresh

  let
    -- Make a map of names to the unification variables of untyped declarations
    untypedDict = zip (map fst untyped) untypedNames
    -- Create the dictionary of all name/type pairs, which will be added to the environment during type checking
    dict = M.fromList (map (\(ident, ty) -> ((moduleName, ident), (ty, LocalVariable, Undefined))) $ typedDict ++ untypedDict)
  return (es, dict, untypedDict)

typeForBindingGroupElement :: ModuleName -> (Ident, (Expr, Maybe (Type, Bool))) -> M.Map (ModuleName, Ident) (Type, NameKind, NameVisibility) -> [(Ident, Type)] -> UnifyT Type Check (Ident, (Expr, Type))
typeForBindingGroupElement moduleName el dict untypedDict =
  -- If the declaration is a function, it has access to other values in the binding group.
  -- If not, the generated code might fail at runtime since those values might be undefined.
  case el of
    -- Typed declarations
    (ident, (val', Just (ty, checkType))) -> do
      -- Kind check
      kind <- liftCheck $ kindOf moduleName ty
      guardWith (strMsg $ "Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
      -- Check the type with the new names in scope
      ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
      val'' <- if checkType
               then bindNames dict $ TypedValue True <$> check val' ty' <*> pure ty'
               else return (TypedValue False val' ty')
      return (ident, (val'', ty'))
    -- Untyped declarations
    (ident, (val', Nothing)) -> do
      -- Infer the type with the new names in scope
      TypedValue _ val'' ty <- bindNames dict $ infer val'
      ty =?= fromMaybe (error "name not found in dictionary") (lookup ident untypedDict)
      return (ident, (TypedValue True val'' ty, ty))

-- |
-- Check if a value contains a type annotation
--
isTyped :: (Ident, Expr) -> (Ident, (Expr, Maybe (Type, Bool)))
isTyped (name, TypedValue checkType value ty) = (name, (value, Just (ty, checkType)))
isTyped (name, value) = (name, (value, Nothing))

-- |
-- Map a function over type annotations appearing inside a value
--
overTypes :: (Type -> Type) -> Expr -> Expr
overTypes f = let (_, f', _) = everywhereOnValues id g id in f'
  where
  g :: Expr -> Expr
  g (TypedValue checkTy val t) = TypedValue checkTy val (f t)
  g (TypeClassDictionary b (nm, tys) sco) = TypeClassDictionary b (nm, map f tys) sco
  g other = other

-- |
-- Replace type class dictionary placeholders with inferred type class dictionaries
--
replaceTypeClassDictionaries :: ModuleName -> Expr -> Check Expr
replaceTypeClassDictionaries mn =
  let (_, f, _) = everywhereOnValuesTopDownM return go return
  in f
  where
  go (TypeClassDictionary trySuperclasses constraint dicts) = do
    env <- getEnv
    entails env mn dicts constraint trySuperclasses
  go other = return other

-- |
-- A simplified representation of expressions which are used to represent type
-- class dictionaries at runtime, which can be compared for equality
--
data DictionaryValue
  -- |
  -- A dictionary which is brought into scope by a local constraint
  --
  = LocalDictionaryValue (Qualified Ident)
  -- |
  -- A dictionary which is brought into scope by an instance declaration
  --
  | GlobalDictionaryValue (Qualified Ident)
  -- |
  -- A dictionary which depends on other dictionaries
  --
  | DependentDictionaryValue (Qualified Ident) [DictionaryValue]
  -- |
  -- A subclass dictionary
  --
  | SubclassDictionaryValue DictionaryValue (Qualified ProperName) Integer
  deriving (Show, Ord, Eq)

-- |
-- Check that the current set of type class dictionaries entail the specified type class goal, and, if so,
-- return a type class dictionary reference.
--
entails :: Environment -> ModuleName -> [TypeClassDictionaryInScope] -> (Qualified ProperName, [Type]) -> Bool -> Check Expr
entails env moduleName context = solve (sortedNubBy canonicalizeDictionary (filter filterModule context))
  where
    sortedNubBy :: (Ord k) => (v -> k) -> [v] -> [v]
    sortedNubBy f vs = M.elems (M.fromList (map (f &&& id) vs))

    -- Filter out type dictionaries which are in scope in the current module
    filterModule :: TypeClassDictionaryInScope -> Bool
    filterModule (TypeClassDictionaryInScope { tcdName = Qualified (Just mn) _ }) | mn == moduleName = True
    filterModule (TypeClassDictionaryInScope { tcdName = Qualified Nothing _ }) = True
    filterModule _ = False

    solve context' (className, tys) trySuperclasses =
      let
        dicts = go trySuperclasses className tys
      in case sortedNubBy dictTrace (chooseSimplestDictionaries dicts) of
           [] -> throwError . strMsg $ "No instance found for " ++ show className ++ " " ++ unwords (map prettyPrintTypeAtom tys)
           [_] -> return $ dictionaryValueToValue $ head dicts
           _ -> throwError . strMsg $ "Overlapping instances found for " ++ show className ++ " " ++ unwords (map prettyPrintTypeAtom tys)
      where
	  go trySuperclasses' className' tys' =
	    -- Look for regular type instances
	    [ mkDictionary (canonicalizeDictionary tcd) args
	    | tcd <- context'
	    -- Make sure the type class name matches the one we are trying to satisfy
	    , className' == tcdClassName tcd
	    -- Make sure the type unifies with the type in the type instance definition
	    , subst <- maybeToList . (>>= verifySubstitution) . fmap concat $ zipWithM (typeHeadsAreEqual moduleName env) tys' (tcdInstanceTypes tcd)
	    -- Solve any necessary subgoals
	    , args <- solveSubgoals subst (tcdDependencies tcd) ] ++

	    -- Look for implementations via superclasses
	    [ SubclassDictionaryValue suDict superclass index
	    | trySuperclasses'
	    , (subclassName, (args, _, implies)) <- M.toList (typeClasses env)
	    -- Try each superclass
	    , (index, (superclass, suTyArgs)) <- zip [0..] implies
	    -- Make sure the type class name matches the superclass name
	    , className' == superclass
	    -- Make sure the types unify with the types in the superclass implication
	    , subst <- maybeToList . (>>= verifySubstitution) . fmap concat $ zipWithM (typeHeadsAreEqual moduleName env) tys' suTyArgs
	    -- Finally, satisfy the subclass constraint
	    , args' <- maybeToList $ mapM (flip lookup subst) args
	    , suDict <- go True subclassName args' ]

	  -- Create dictionaries for subgoals which still need to be solved by calling go recursively
	  -- E.g. the goal (Show a, Show b) => Show (Either a b) can be satisfied if the current type
	  -- unifies with Either a b, and we can satisfy the subgoals Show a and Show b recursively.
	  solveSubgoals :: [(String, Type)] -> Maybe [(Qualified ProperName, [Type])] -> [Maybe [DictionaryValue]]
	  solveSubgoals _ Nothing = return Nothing
	  solveSubgoals subst (Just subgoals) = do
	    dict <- mapM (uncurry (go True) . second (map (replaceAllTypeVars subst))) subgoals
	    return $ Just dict
	  -- Make a dictionary from subgoal dictionaries by applying the correct function
	  mkDictionary :: Qualified Ident -> Maybe [DictionaryValue] -> DictionaryValue
	  mkDictionary fnName Nothing = LocalDictionaryValue fnName
	  mkDictionary fnName (Just []) = GlobalDictionaryValue fnName
	  mkDictionary fnName (Just dicts) = DependentDictionaryValue fnName dicts
	  -- Turn a DictionaryValue into a Expr
	  dictionaryValueToValue :: DictionaryValue -> Expr
	  dictionaryValueToValue (LocalDictionaryValue fnName) = Var fnName
	  dictionaryValueToValue (GlobalDictionaryValue fnName) = App (Var fnName) valUndefined
	  dictionaryValueToValue (DependentDictionaryValue fnName dicts) = foldl App (Var fnName) (map dictionaryValueToValue dicts)
	  dictionaryValueToValue (SubclassDictionaryValue dict superclassName index) =
	    App (Accessor (C.__superclass_ ++ show superclassName ++ "_" ++ show index)
	                  (dictionaryValueToValue dict))
	        valUndefined
	  -- Ensure that a substitution is valid
	  verifySubstitution :: [(String, Type)] -> Maybe [(String, Type)]
	  verifySubstitution subst = do
	    let grps = groupBy ((==) `on` fst) subst
	    guard (all (pairwise (unifiesWith env) . map snd) grps)
	    return $ map head grps
	  -- Choose the simplest DictionaryValues from a list of candidates
	  -- The reason for this function is as follows:
	  -- When considering overlapping instances, we don't want to consider the same dictionary
	  -- to be an overlap of itself when obtained as a superclass of another class.
	  -- Observing that we probably don't want to select a superclass instance when an instance
	  -- is available directly, and that there is no way for a superclass instance to actually
	  -- introduce an overlap that wouldn't have been there already, we simply remove dictionaries
	  -- obtained as superclass instances if there are simpler instances available.
	  chooseSimplestDictionaries :: [DictionaryValue] -> [DictionaryValue]
	  chooseSimplestDictionaries ds = case filter isSimpleDictionaryValue ds of
	                                    [] -> ds
	                                    simple -> simple
	  isSimpleDictionaryValue SubclassDictionaryValue{} = False
	  isSimpleDictionaryValue (DependentDictionaryValue _ ds) = all isSimpleDictionaryValue ds
	  isSimpleDictionaryValue _ = True
	  -- |
	  -- Get the "trace" of a DictionaryValue - that is, remove all SubclassDictionaryValue
	  -- data constructors
	  --
	  dictTrace :: DictionaryValue -> DictionaryValue
	  dictTrace (DependentDictionaryValue fnName dicts) = DependentDictionaryValue fnName $ map dictTrace dicts
	  dictTrace (SubclassDictionaryValue dict _ _) = dictTrace dict
	  dictTrace other = other

    valUndefined :: Expr
    valUndefined = Var (Qualified (Just (ModuleName [ProperName C.prim])) (Ident C.undefined))

-- |
-- Check all values in a list pairwise match a predicate
--
pairwise :: (a -> a -> Bool) -> [a] -> Bool
pairwise _ [] = True
pairwise _ [_] = True
pairwise p (x : xs) = all (p x) xs && pairwise p xs

-- |
-- Check that two types unify
--
unifiesWith :: Environment -> Type -> Type -> Bool
unifiesWith _ (TUnknown u1) (TUnknown u2) | u1 == u2 = True
unifiesWith _ (Skolem _ s1 _) (Skolem _ s2 _) | s1 == s2 = True
unifiesWith _ (TypeVar v1) (TypeVar v2) | v1 == v2 = True
unifiesWith _ (TypeConstructor c1) (TypeConstructor c2) | c1 == c2 = True
unifiesWith e (TypeApp h1 t1) (TypeApp h2 t2) = unifiesWith e h1 h2 && unifiesWith e t1 t2
unifiesWith e (SaturatedTypeSynonym name args) t2 =
  case expandTypeSynonym' e name args of
    Left  _  -> False
    Right t1 -> unifiesWith e t1 t2
unifiesWith e t1 t2@(SaturatedTypeSynonym _ _) = unifiesWith e t2 t1
unifiesWith _ _ _ = False

-- |
-- Check whether the type heads of two types are equal (for the purposes of type class dictionary lookup),
-- and return a substitution from type variables to types which makes the type heads unify.
--
typeHeadsAreEqual :: ModuleName -> Environment -> Type -> Type -> Maybe [(String, Type)]
typeHeadsAreEqual _ _ (Skolem _ s1 _) (Skolem _ s2 _) | s1 == s2 = Just []
typeHeadsAreEqual _ _ t (TypeVar v) = Just [(v, t)]
typeHeadsAreEqual _ _ (TypeConstructor c1) (TypeConstructor c2) | c1 == c2 = Just []
typeHeadsAreEqual m e (TypeApp h1 t1) (TypeApp h2 t2) = (++) <$> typeHeadsAreEqual m e h1 h2 <*> typeHeadsAreEqual m e t1 t2
typeHeadsAreEqual m e (SaturatedTypeSynonym name args) t2 = case expandTypeSynonym' e name args of
  Left  _  -> Nothing
  Right t1 -> typeHeadsAreEqual m e t1 t2
typeHeadsAreEqual _ _ _ _ = Nothing

-- |
-- Ensure skolem variables do not escape their scope
--
skolemEscapeCheck :: Expr -> Check ()
skolemEscapeCheck (TypedValue False _ _) = return ()
skolemEscapeCheck root@TypedValue{} =
  -- Every skolem variable is created when a ForAll type is skolemized.
  -- This determines the scope of that skolem variable, which is copied from the SkolemScope
  -- field of the ForAll constructor.
  -- We traverse the tree top-down, and collect any SkolemScopes introduced by ForAlls.
  -- If a Skolem is encountered whose SkolemScope is not in the current list, we have found
  -- an escaped skolem variable.
  let (_, f, _, _, _) = everythingWithContextOnValues [] [] (++) def go def def def
  in case f root of
       [] -> return ()
       ((binding, val) : _) -> throwError $ mkErrorStack ("Rigid/skolem type variable " ++ maybe "" (("bound by " ++) . prettyPrintValue) binding ++ " has escaped.") (Just (ExprError val))
  where
  def s _ = (s, [])

  go :: [(SkolemScope, Expr)] -> Expr -> ([(SkolemScope, Expr)], [(Maybe Expr, Expr)])
  go scos val@(TypedValue _ _ (ForAll _ _ (Just sco))) = ((sco, val) : scos, [])
  go scos val@(TypedValue _ _ ty) = case collectSkolems ty \\ map fst scos of
                                      (sco : _) -> (scos, [(findBindingScope sco, val)])
                                      _ -> (scos, [])
    where
    collectSkolems :: Type -> [SkolemScope]
    collectSkolems = nub . everythingOnTypes (++) collect
      where
      collect (Skolem _ _ scope) = [scope]
      collect _ = []
  go scos _ = (scos, [])
  findBindingScope :: SkolemScope -> Maybe Expr
  findBindingScope sco =
    let (_, f, _, _, _) = everythingOnValues mappend (const mempty) go' (const mempty) (const mempty) (const mempty)
    in getFirst $ f root
    where
    go' val@(TypedValue _ _ (ForAll _ _ (Just sco'))) | sco == sco' = First (Just val)
    go' _ = mempty
skolemEscapeCheck val = throwError $ mkErrorStack "Untyped value passed to skolemEscapeCheck" (Just (ExprError val))

-- |
-- Ensure a row contains no duplicate labels
--
setify :: Type -> Type
setify = rowFromList . first (M.toList . M.fromList) . rowToList

-- |
-- \"Setify\" all rows occuring inside a value
--
setifyAll :: Type -> Type
setifyAll = everywhereOnTypes setify

-- |
-- Replace outermost unsolved unification variables with named type variables
--
varIfUnknown :: Type -> Type
varIfUnknown ty =
  let unks = nub $ unknowns ty
      toName = (:) 't' . show
      ty' = everywhereOnTypes typeToVar ty
      typeToVar :: Type -> Type
      typeToVar (TUnknown u) = TypeVar (toName u)
      typeToVar t = t
  in mkForAll (sort . map toName $ unks) ty'

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
   (_, ty') <- instantiatePolyTypeWithUnknowns (error "Types under a constraint cannot themselves be constrained") ty
   return (foldl App val (map (flip (TypeClassDictionary True) dicts) constraints), ty')
instantiatePolyTypeWithUnknowns val ty = return (val, ty)

-- |
-- Replace a single type variable with a new unification variable
--
replaceVarWithUnknown :: String -> Type -> UnifyT Type Check Type
replaceVarWithUnknown ident ty = do
  tu <- fresh
  return $ replaceTypeVars ident tu ty

-- |
-- Replace fully applied type synonyms with the @SaturatedTypeSynonym@ data constructor, which helps generate
-- better error messages during unification.
--
replaceAllTypeSynonyms' :: Environment -> Type -> Either String Type
replaceAllTypeSynonyms' env d =
  let
    syns = map (\(name, (args, _)) -> (name, length args)) . M.toList $ typeSynonyms env
  in
    saturateAllTypeSynonyms syns d

replaceAllTypeSynonyms :: (Error e, Functor m, Monad m, MonadState CheckState m, MonadError e m) => Type -> m Type
replaceAllTypeSynonyms d = do
  env <- getEnv
  either (throwError . strMsg) return $ replaceAllTypeSynonyms' env d

-- |
-- \"Desaturate\" @SaturatedTypeSynonym@s
--
desaturateAllTypeSynonyms :: Type -> Type
desaturateAllTypeSynonyms = everywhereOnTypes replaceSaturatedTypeSynonym
  where
  replaceSaturatedTypeSynonym (SaturatedTypeSynonym name args) = foldl TypeApp (TypeConstructor name) args
  replaceSaturatedTypeSynonym t = t

-- |
-- Replace a type synonym and its arguments with the aliased type
--
expandTypeSynonym' :: Environment -> Qualified ProperName -> [Type] -> Either String Type
expandTypeSynonym' env name args =
  case M.lookup name (typeSynonyms env) of
    Just (synArgs, body) -> do
      let repl = replaceAllTypeVars (zip synArgs args) body
      replaceAllTypeSynonyms' env repl
    Nothing -> error "Type synonym was not defined"

expandTypeSynonym :: (Error e, Functor m, Monad m, MonadState CheckState m, MonadError e m) => Qualified ProperName -> [Type] -> m Type
expandTypeSynonym name args = do
  env <- getEnv
  either (throwError . strMsg) return $ expandTypeSynonym' env name args

expandAllTypeSynonyms :: (Error e, Functor m, Applicative m, Monad m, MonadState CheckState m, MonadError e m) => Type -> m Type
expandAllTypeSynonyms = everywhereOnTypesTopDownM go
  where
  go (SaturatedTypeSynonym name args) = expandTypeSynonym name args
  go other = return other

-- |
-- Ensure a set of property names and value does not contain duplicate labels
--
ensureNoDuplicateProperties :: (Error e, MonadError e m) => [(String, Expr)] -> m ()
ensureNoDuplicateProperties ps = guardWith (strMsg "Duplicate property names") $ length (nub . map fst $ ps) == length ps

-- |
-- Infer a type for a value, rethrowing any error to provide a more useful error message
--
infer :: Expr -> UnifyT Type Check Expr
infer val = rethrow (mkErrorStack "Error inferring type of value" (Just (ExprError val)) <>) $ infer' val

-- |
-- Infer a type for a value
--
infer' :: Expr -> UnifyT Type Check Expr
infer' v@(NumericLiteral _) = return $ TypedValue True v tyNumber
infer' v@(StringLiteral _) = return $ TypedValue True v tyString
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
infer' (Accessor prop val) = do
  typed@(TypedValue _ _ objTy) <- infer val
  propTy <- inferProperty objTy prop
  case propTy of
    Nothing -> do
      field <- fresh
      rest <- fresh
      _ <- subsumes Nothing objTy (TypeApp tyObject (RCons prop field rest))
      return $ TypedValue True (Accessor prop typed) field
    Just ty -> return $ TypedValue True (Accessor prop typed) ty
infer' (Abs (Left arg) ret) = do
  ty <- fresh
  Just moduleName <- checkCurrentModule <$> get
  makeBindingGroupVisible $ bindLocalVariables moduleName [(arg, ty, Defined)] $ do
    body@(TypedValue _ _ bodyTy) <- infer' ret
    return $ TypedValue True (Abs (Left arg) body) $ function ty bodyTy
infer' (Abs (Right _) _) = error "Binder was not desugared"
infer' (App f arg) = do
  f'@(TypedValue _ _ ft) <- infer f
  (ret, app) <- checkFunctionApplication f' ft arg Nothing
  return $ TypedValue True app ret
infer' (Var var) = do
  Just moduleName <- checkCurrentModule <$> get
  checkVisibility moduleName var
  ty <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable moduleName $ var
  case ty of
    ConstrainedType constraints ty' -> do
      dicts <- getTypeClassDictionaries
      return $ TypedValue True (foldl App (Var var) (map (flip (TypeClassDictionary True) dicts) constraints)) ty'
    _ -> return $ TypedValue True (Var var) ty
infer' v@(Constructor c) = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . strMsg $ "Constructor " ++ show c ++ " is undefined"
    Just (_, _, ty) -> do ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
                          return $ TypedValue True v ty'
infer' (Case vals binders) = do
  ts <- mapM infer vals
  ret <- fresh
  binders' <- checkBinders (map (\(TypedValue _ _ t) -> t) ts) ret binders
  return $ TypedValue True (Case ts binders') ret
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
  return $ TypeClassDictionary False (className, tys) dicts
infer' (TypedValue checkType val ty) = do
  Just moduleName <- checkCurrentModule <$> get
  kind <- liftCheck $ kindOf moduleName ty
  guardWith (strMsg $ "Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  val' <- if checkType then check val ty' else return val
  return $ TypedValue True val' ty'
infer' (PositionedValue pos val) = rethrowWithPosition pos $ infer' val
infer' _ = error "Invalid argument to infer"

inferLetBinding :: [Declaration] -> [Declaration] -> Expr -> (Expr -> UnifyT Type Check Expr) -> UnifyT Type Check ([Declaration], Expr)
inferLetBinding seen [] ret j = (,) seen <$> makeBindingGroupVisible (j ret)
inferLetBinding seen (ValueDeclaration ident nameKind [] Nothing tv@(TypedValue checkType val ty) : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  kind <- liftCheck $ kindOf moduleName ty
  guardWith (strMsg $ "Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  let dict = M.singleton (moduleName, ident) (ty, nameKind, Undefined)
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  TypedValue _ val' ty'' <- if checkType then bindNames dict (check val ty') else return tv
  bindNames (M.singleton (moduleName, ident) (ty'', nameKind, Defined)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] Nothing (TypedValue checkType val' ty'')]) rest ret j
inferLetBinding seen (ValueDeclaration ident nameKind [] Nothing val : rest) ret j = do
  valTy <- fresh
  Just moduleName <- checkCurrentModule <$> get
  let dict = M.singleton (moduleName, ident) (valTy, nameKind, Undefined)
  TypedValue _ val' valTy' <- bindNames dict $ infer val
  valTy =?= valTy'
  bindNames (M.singleton (moduleName, ident) (valTy', nameKind, Defined)) $ inferLetBinding (seen ++ [ValueDeclaration ident nameKind [] Nothing val']) rest ret j
inferLetBinding seen (BindingGroupDeclaration ds : rest) ret j = do
  Just moduleName <- checkCurrentModule <$> get
  (es, dict, untypedDict) <- typeDictionaryForBindingGroup moduleName (map (\(i, _, v) -> (i, v)) ds)
  ds' <- forM es $ \e -> do
    (ident, (val', _)) <- typeForBindingGroupElement moduleName e dict untypedDict
    return $ (ident, LocalVariable, val')
  makeBindingGroupVisible $ bindNames dict $ inferLetBinding (seen ++ [BindingGroupDeclaration ds']) rest ret j
inferLetBinding seen (PositionedDeclaration pos d : ds) ret j = rethrowWithPosition pos $ do
  ((d' : ds'), val') <- inferLetBinding seen (d : ds) ret j
  return (PositionedDeclaration pos d' : ds', val')
inferLetBinding _ _ _ _ = error "Invalid argument to inferLetBinding"

-- |
-- Infer the type of a property inside a record with a given type
--
inferProperty :: Type -> String -> UnifyT Type Check (Maybe Type)
inferProperty (TypeApp obj row) prop | obj == tyObject = do
  let (props, _) = rowToList row
  return $ lookup prop props
inferProperty (SaturatedTypeSynonym name args) prop = do
  replaced <- introduceSkolemScope <=< expandTypeSynonym name $ args
  inferProperty replaced prop
inferProperty (ForAll ident ty _) prop = do
  replaced <- replaceVarWithUnknown ident ty
  inferProperty replaced prop
inferProperty _ _ = return Nothing

-- |
-- Infer the types of variables brought into scope by a binder
--
inferBinder :: Type -> Binder -> UnifyT Type Check (M.Map Ident Type)
inferBinder _ NullBinder = return M.empty
inferBinder val (StringBinder _) = val =?= tyString >> return M.empty
inferBinder val (NumberBinder _) = val =?= tyNumber >> return M.empty
inferBinder val (BooleanBinder _) = val =?= tyBoolean >> return M.empty
inferBinder val (VarBinder name) = return $ M.singleton name val
inferBinder val (ConstructorBinder ctor binders) = do
  env <- getEnv
  case M.lookup ctor (dataConstructors env) of
    Just (_, _, ty) -> do
      (_, fn) <- instantiatePolyTypeWithUnknowns (error "Data constructor types cannot contain constraints") ty
      fn' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ fn
      go binders fn'
        where
        go [] ty' = do
          _ <- subsumes Nothing val ty'
          return M.empty
        go (binder : binders') (TypeApp (TypeApp t obj) ret) | t == tyFunction =
          M.union <$> inferBinder obj binder <*> go binders' ret
        go _ _ = throwError . strMsg $ "Wrong number of arguments to constructor " ++ show ctor
    _ -> throwError . strMsg $ "Constructor " ++ show ctor ++ " is not defined"
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
inferBinder val (ConsBinder headBinder tailBinder) = do
  el <- fresh
  m1 <- inferBinder el headBinder
  m2 <- inferBinder val tailBinder
  val =?= TypeApp tyArray el
  return $ m1 `M.union` m2
inferBinder val (NamedBinder name binder) = do
  m <- inferBinder val binder
  return $ M.insert name val m
inferBinder val (PositionedBinder pos binder) =
  rethrowWithPosition pos $ inferBinder val binder

-- |
-- Check the types of the return values in a set of binders in a case statement
--
checkBinders :: [Type] -> Type -> [CaseAlternative] -> UnifyT Type Check [CaseAlternative]
checkBinders _ _ [] = return []
checkBinders nvals ret (CaseAlternative binders grd val : bs) = do
  Just moduleName <- checkCurrentModule <$> get
  m1 <- M.unions <$> zipWithM inferBinder nvals binders
  r <- bindLocalVariables moduleName [ (name, ty, Defined) | (name, ty) <- M.toList m1 ] $ do
    val' <- TypedValue True <$> check val ret <*> pure ret
    case grd of
      Nothing -> return $ CaseAlternative binders Nothing val'
      Just g -> do
        g' <- check g tyBoolean
        return $ CaseAlternative binders (Just g') val'
  rs <- checkBinders nvals ret bs
  return $ r : rs

-- |
-- Generate a new skolem constant
--
newSkolemConstant :: UnifyT Type Check Int
newSkolemConstant = fresh'

-- |
-- Generate a new skolem scope
--
newSkolemScope :: UnifyT Type Check SkolemScope
newSkolemScope = SkolemScope <$> fresh'

-- |
-- Skolemize a type variable by replacing its instances with fresh skolem constants
--
skolemize :: String -> Int -> SkolemScope -> Type -> Type
skolemize ident sko scope = replaceTypeVars ident (Skolem ident sko scope)

-- |
-- This function has one purpose - to skolemize type variables appearing in a
-- SuperClassDictionary placeholder. These type variables are somewhat unique since they are the
-- only example of scoped type variables.
--
skolemizeTypesInValue :: String -> Int -> SkolemScope -> Expr -> Expr
skolemizeTypesInValue ident sko scope = let (_, f, _) = everywhereOnValues id go id in f
  where
  go (SuperClassDictionary c ts) = SuperClassDictionary c (map (skolemize ident sko scope) ts)
  go other = other

-- |
-- Introduce skolem scope at every occurence of a ForAll
--
introduceSkolemScope :: Type -> UnifyT Type Check Type
introduceSkolemScope = everywhereOnTypesM go
  where
  go (ForAll ident ty Nothing) = ForAll ident ty <$> (Just <$> newSkolemScope)
  go other = return other

-- |
-- Check the type of a value, rethrowing errors to provide a better error message
--
check :: Expr -> Type -> UnifyT Type Check Expr
check val ty = rethrow (mkErrorStack errorMessage (Just (ExprError val)) <>) $ check' val ty
  where
  errorMessage =
    "Error checking type of term " ++
    prettyPrintValue val ++
    " against type " ++
    prettyPrintType ty

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
  val' <- makeBindingGroupVisible $ withTypeClassDictionaries (zipWith (\name (className, instanceTy) ->
    TypeClassDictionaryInScope name className instanceTy Nothing TCDRegular) (map (Qualified Nothing) dictNames)
      constraints) $ check val ty
  return $ TypedValue True (foldr (Abs . Left) val' dictNames) t
check' val (SaturatedTypeSynonym name args) = do
  ty <- introduceSkolemScope <=< expandTypeSynonym name $ args
  check val ty
check' val u@(TUnknown _) = do
  val'@(TypedValue _ _ ty) <- infer val
  -- Don't unify an unknown with an inferred polytype
  (val'', ty') <- instantiatePolyTypeWithUnknowns val' ty
  ty' =?= u
  return $ TypedValue True val'' ty'
check' v@(NumericLiteral _) t | t == tyNumber =
  return $ TypedValue True v t
check' v@(StringLiteral _) t | t == tyString =
  return $ TypedValue True v t
check' v@(BooleanLiteral _) t | t == tyBoolean =
  return $ TypedValue True v t
check' (ArrayLiteral vals) t@(TypeApp a ty) = do
  a =?= tyArray
  array <- ArrayLiteral <$> forM vals (`check` ty)
  return $ TypedValue True array t
check' (Abs (Left arg) ret) ty@(TypeApp (TypeApp t argTy) retTy) | t == tyFunction = do
  Just moduleName <- checkCurrentModule <$> get
  ret' <- makeBindingGroupVisible $ bindLocalVariables moduleName [(arg, argTy, Defined)] $ check ret retTy
  return $ TypedValue True (Abs (Left arg) ret') ty
check' (Abs (Right _) _) _ = error "Binder was not desugared"
check' (App f arg) ret = do
  f'@(TypedValue _ _ ft) <- infer f
  (_, app) <- checkFunctionApplication f' ft arg (Just ret)
  return $ TypedValue True app ret
check' v@(Var var) ty = do
  Just moduleName <- checkCurrentModule <$> get
  checkVisibility moduleName var
  repl <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable moduleName $ var
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  v' <- subsumes (Just v) repl ty'
  case v' of
    Nothing -> throwError . strMsg $ "Unable to check type subsumption"
    Just v'' -> return $ TypedValue True v'' ty'
check' (SuperClassDictionary className tys) _ = do
  {-
  -- Here, we replace a placeholder for a superclass dictionary with a regular
  -- TypeClassDictionary placeholder. The reason we do this is that it is necessary to have the
  -- correct super instance dictionaries in scope, and these are not available when the type class
  -- declaration gets desugared.
  --
  -- Note also that the first argument to TypeClassDictionary is False, meaning we _do not_ want
  -- to consider superclass instances when searching for this dictionary - doing so might lead
  -- to traversing a cycle in the instance graph.
  -}
  dicts <- getTypeClassDictionaries
  return $ TypeClassDictionary False (className, tys) dicts
check' (TypedValue checkType val ty1) ty2 = do
  Just moduleName <- checkCurrentModule <$> get
  kind <- liftCheck $ kindOf moduleName ty1
  guardWith (strMsg $ "Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty1' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
  ty2' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty2
  val' <- subsumes (Just val) ty1' ty2'
  case val' of
    Nothing -> throwError . strMsg $ "Unable to check type subsumption"
    Just val'' -> do
      val''' <- if checkType then check val'' ty2' else return val''
      return $ TypedValue checkType (TypedValue True val''' ty1') ty2'
check' (Case vals binders) ret = do
  vals' <- mapM infer vals
  let ts = map (\(TypedValue _ _ t) -> t) vals'
  binders' <- checkBinders ts ret binders
  return $ TypedValue True (Case vals' binders') ret
check' (IfThenElse cond th el) ty = do
  cond' <- check cond tyBoolean
  th' <- check th ty
  el' <- check el ty
  return $ TypedValue True (IfThenElse cond' th' el') ty
check' (ObjectLiteral ps) t@(TypeApp obj row) | obj == tyObject = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties ps row False
  return $ TypedValue True (ObjectLiteral ps') t
check' (TypeClassDictionaryConstructorApp name ps) t = do
  ps' <- check' ps t
  return $ TypedValue True (TypeClassDictionaryConstructorApp name ps') t
check' (ObjectUpdate obj ps) t@(TypeApp o row) | o == tyObject = do
  ensureNoDuplicateProperties ps
  us <- zip (map fst ps) <$> replicateM (length ps) fresh
  let (propsToCheck, rest) = rowToList row
      propsToRemove = map fst ps
      remainingProps = filter (\(p, _) -> p `notElem` propsToRemove) propsToCheck
  obj' <- check obj (TypeApp tyObject (rowFromList (us ++ remainingProps, rest)))
  ps' <- checkProperties ps row True
  return $ TypedValue True (ObjectUpdate obj' ps') t
check' (Accessor prop val) ty = do
  rest <- fresh
  val' <- check val (TypeApp tyObject (RCons prop ty rest))
  return $ TypedValue True (Accessor prop val') ty
check' (Constructor c) ty = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . strMsg $ "Constructor " ++ show c ++ " is undefined"
    Just (_, _, ty1) -> do
      repl <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
      _ <- subsumes Nothing repl ty
      return $ TypedValue True (Constructor c) ty
check' (Let ds val) ty = do
  (ds', val') <- inferLetBinding [] ds val (flip check ty)
  return $ TypedValue True (Let ds' val') ty
check' val ty | containsTypeSynonyms ty = do
  ty' <- introduceSkolemScope <=< expandAllTypeSynonyms $ ty
  check val ty'
check' (PositionedValue pos val) ty =
  rethrowWithPosition pos $ check val ty
check' val ty = throwError $ mkErrorStack ("Expr does not have type " ++ prettyPrintType ty) (Just (ExprError val))

containsTypeSynonyms :: Type -> Bool
containsTypeSynonyms = everythingOnTypes (||) go where
  go (SaturatedTypeSynonym _ _) = True
  go _ = False

-- |
-- Check the type of a collection of named record fields
--
-- The @lax@ parameter controls whether or not every record member has to be provided. For object updates, this is not the case.
--
checkProperties :: [(String, Expr)] -> Type -> Bool -> UnifyT Type Check [(String, Expr)]
checkProperties ps row lax = let (ts, r') = rowToList row in go ps ts r' where
  go [] [] REmpty = return []
  go [] [] u@(TUnknown _) = do u =?= REmpty
                               return []
  go [] [] (Skolem _ _ _) | lax = return []
  go [] ((p, _): _) _ | lax = return []
                      | otherwise = throwError $ mkErrorStack ("Object does not have property " ++ p) (Just (ExprError (ObjectLiteral ps)))
  go ((p,_):_) [] REmpty = throwError $ mkErrorStack ("Property " ++ p ++ " is not present in closed object type " ++ prettyPrintRow row) (Just (ExprError (ObjectLiteral ps)))
  go ((p,v):ps') [] u@(TUnknown _) = do
    v'@(TypedValue _ _ ty) <- infer v
    rest <- fresh
    u =?= RCons p ty rest
    ps'' <- go ps' [] rest
    return $ (p, v') : ps''
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
  go _ _ _ = throwError $ mkErrorStack ("Object does not have type " ++ prettyPrintType (TypeApp tyObject row)) (Just (ExprError (ObjectLiteral ps)))

-- |
-- Check the type of a function application, rethrowing errors to provide a better error message
--
checkFunctionApplication :: Expr -> Type -> Expr -> Maybe Type -> UnifyT Type Check (Type, Expr)
checkFunctionApplication fn fnTy arg ret = rethrow (mkErrorStack errorMessage (Just (ExprError fn)) <>) $ do
  subst <- unifyCurrentSubstitution <$> UnifyT get
  checkFunctionApplication' fn (subst $? fnTy) arg (($?) subst <$> ret)
  where
  errorMessage = "Error applying function of type "
    ++ prettyPrintType fnTy
    ++ " to argument " ++ prettyPrintValue arg

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
checkFunctionApplication' fn (SaturatedTypeSynonym name tyArgs) arg ret = do
  ty <- introduceSkolemScope <=< expandTypeSynonym name $ tyArgs
  checkFunctionApplication fn ty arg ret
checkFunctionApplication' fn (ConstrainedType constraints fnTy) arg ret = do
  dicts <- getTypeClassDictionaries
  checkFunctionApplication' (foldl App fn (map (flip (TypeClassDictionary True) dicts) constraints)) fnTy arg ret
checkFunctionApplication' fn fnTy dict@(TypeClassDictionary _ _ _) _ =
  return (fnTy, App fn dict)
checkFunctionApplication' _ fnTy arg _ = throwError . strMsg $ "Cannot apply a function of type "
  ++ prettyPrintType fnTy
  ++ " to argument " ++ prettyPrintValue arg

-- |
-- Check whether one type subsumes another, rethrowing errors to provide a better error message
--
subsumes :: Maybe Expr -> Type -> Type -> UnifyT Type Check (Maybe Expr)
subsumes val ty1 ty2 = rethrow (mkErrorStack errorMessage (ExprError <$> val) <>) $ subsumes' val ty1 ty2
  where
  errorMessage = "Error checking that type "
    ++ prettyPrintType ty1
    ++ " subsumes type "
    ++ prettyPrintType ty2

-- |
-- Check whether one type subsumes another
--
subsumes' :: Maybe Expr -> Type -> Type -> UnifyT Type Check (Maybe Expr)
subsumes' val (ForAll ident ty1 _) ty2 = do
  replaced <- replaceVarWithUnknown ident ty1
  subsumes val replaced ty2
subsumes' val ty1 (ForAll ident ty2 sco) =
  case sco of
    Just sco' -> do
      sko <- newSkolemConstant
      let sk = skolemize ident sko sco' ty2
      subsumes val ty1 sk
    Nothing -> throwError . strMsg $ "Skolem variable scope is unspecified"
subsumes' val (TypeApp (TypeApp f1 arg1) ret1) (TypeApp (TypeApp f2 arg2) ret2) | f1 == tyFunction && f2 == tyFunction = do
  _ <- subsumes Nothing arg2 arg1
  _ <- subsumes Nothing ret1 ret2
  return val
subsumes' val (SaturatedTypeSynonym name tyArgs) ty2 = do
  ty1 <- introduceSkolemScope <=< expandTypeSynonym name $ tyArgs
  subsumes val ty1 ty2
subsumes' val ty1 (SaturatedTypeSynonym name tyArgs) = do
  ty2 <- introduceSkolemScope <=< expandTypeSynonym name $ tyArgs
  subsumes val ty1 ty2
subsumes' (Just val) (ConstrainedType constraints ty1) ty2 = do
  dicts <- getTypeClassDictionaries
  subsumes' (Just $ foldl App val (map (flip (TypeClassDictionary True) dicts) constraints)) ty1 ty2
subsumes' val (TypeApp f1 r1) (TypeApp f2 r2) | f1 == tyObject && f2 == tyObject = do
  let
    (ts1, r1') = rowToList r1
    (ts2, r2') = rowToList r2
    ts1' = sortBy (comparing fst) ts1
    ts2' = sortBy (comparing fst) ts2
  go ts1' ts2' r1' r2'
  return val
  where
  go [] ts2 r1' r2' = r1' =?= rowFromList (ts2, r2')
  go ts1 [] r1' r2' = r2' =?= rowFromList (ts1, r1')
  go ((p1, ty1) : ts1) ((p2, ty2) : ts2) r1' r2'
    | p1 == p2 = do _ <- subsumes Nothing ty1 ty2
                    go ts1 ts2 r1' r2'
    | p1 < p2 = do rest <- fresh
                   r2' =?= RCons p1 ty1 rest
                   go ts1 ((p2, ty2) : ts2) r1' rest
    | otherwise = do rest <- fresh
                     r1' =?= RCons p2 ty2 rest
                     go ((p1, ty1) : ts1) ts2 rest r2'
subsumes' val ty1 ty2@(TypeApp obj _) | obj == tyObject = subsumes val ty2 ty1
subsumes' val ty1 ty2 = do
  ty1 =?= ty2
  return val

-- |
-- Compute the meet of two types, i.e. the most general type which both types subsume.
-- TODO: handle constrained types
--
meet :: Expr -> Expr -> Type -> Type -> UnifyT Type Check (Expr, Expr, Type)
meet e1 e2 t1@(ForAll _ _ _) t2 = meet e1 e2 t1 t2
meet e1 e2 t1 (ForAll ident t2 _) = do
  t2' <- replaceVarWithUnknown ident t2
  meet e1 e2 t1 t2'
meet e1 e2 t1 t2 = do
  t1 =?= t2
  return (e1, e2, t1)




