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
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.TypeChecker.Types (
    typeOf
) where

import Data.List
import Data.Maybe (isJust, fromMaybe)
import Data.Function
import qualified Data.Data as D
import Data.Generics
       (something, everywhere, everywhereM, everything, everywhereBut,
        mkT, mkM, mkQ, extM, extQ)

import Language.PureScript.Values
import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Kinds
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.Pretty
import Language.PureScript.Unknown

import Control.Monad.State
import Control.Monad.Error

import Control.Applicative
import Control.Arrow (Arrow(..), Kleisli(..), (***), (&&&), second)
import qualified Control.Category as C

import qualified Data.Map as M

instance Unifiable Type where
  unknown = TUnknown
  isUnknown (TUnknown u) = Just u
  isUnknown _ = Nothing
  (~~) = unifyTypes
  apply s (TUnknown u) = runSubstitution s u
  apply s (SaturatedTypeSynonym name tys) = SaturatedTypeSynonym name $ map (apply s) tys
  apply s (ForAll idents ty) = ForAll idents $ apply s ty
  apply s (Array t) = Array (apply s t)
  apply s (Object r) = Object (apply s r)
  apply s (Function args ret) = Function (map (apply s) args) (apply s ret)
  apply s (TypeApp t1 t2) = TypeApp (apply s t1) (apply s t2)
  apply _ t = t
  unknowns (TUnknown (Unknown u)) = [u]
  unknowns (SaturatedTypeSynonym _ tys) = concatMap unknowns tys
  unknowns (ForAll idents ty) = unknowns ty
  unknowns (Array t) = unknowns t
  unknowns (Object r) = unknowns r
  unknowns (Function args ret) = concatMap unknowns args ++ unknowns ret
  unknowns (TypeApp t1 t2) = unknowns t1 ++ unknowns t2
  unknowns _ = []

instance Unifiable Row where
  unknown = RUnknown
  isUnknown (RUnknown u) = Just u
  isUnknown _ = Nothing
  r1 ~~ r2 =
      let
        (s1, r1') = rowToList r1
        (s2, r2') = rowToList r2
        int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
        sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
        sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
      in do
        forM_ int (uncurry (~~))
        unifyRows sd1 r1' sd2 r2'
      where
      unifyRows :: [(String, Type)] -> Row -> [(String, Type)] -> Row -> Subst Check ()
      unifyRows [] (RUnknown u) sd r = replace u (rowFromList (sd, r))
      unifyRows sd r [] (RUnknown u) = replace u (rowFromList (sd, r))
      unifyRows ns@((name, ty):row) r others u@(RUnknown un) = do
        occursCheck un ty
        forM row $ \(_, ty) -> occursCheck un ty
        u' <- fresh
        u ~~ RCons name ty u'
        unifyRows row r others u'
      unifyRows [] REmpty [] REmpty = return ()
      unifyRows [] (RowVar v1) [] (RowVar v2) | v1 == v2 = return ()
      unifyRows [] (RSkolem s1) [] (RSkolem s2) | s1 == s2 = return ()
      unifyRows sd1 r1 sd2 r2 = throwError $ "Cannot unify " ++ prettyPrintRow (rowFromList (sd1, r1)) ++ " with " ++ prettyPrintRow (rowFromList (sd2, r2)) ++ "."
  apply s (RUnknown u) = runSubstitution s u
  apply s (RCons name ty r) = RCons name (apply s ty) (apply s r)
  apply _ r = r
  unknowns (RUnknown (Unknown u)) = [u]
  unknowns (RCons _ ty r) = unknowns ty ++ unknowns r
  unknowns _ = []

unifyTypes :: Type -> Type -> Subst Check ()
unifyTypes t1 t2 = rethrow (\e -> "Error unifying type " ++ prettyPrintType t1 ++ " with type " ++ prettyPrintType t2 ++ ":\n" ++ e) $ do
  unifyTypes' t1 t2
  where
  unifyTypes' (TUnknown u1) (TUnknown u2) | u1 == u2 = return ()
  unifyTypes' (TUnknown u) t = replace u t
  unifyTypes' t (TUnknown u) = replace u t
  unifyTypes' (SaturatedTypeSynonym name1 args1) (SaturatedTypeSynonym name2 args2)
    | name1 == name2 = zipWithM_ unifyTypes args1 args2
  unifyTypes' (SaturatedTypeSynonym name args) ty = do
    ty1 <- expandTypeSynonym name args
    ty1 `unifyTypes` ty
  unifyTypes' ty s@(SaturatedTypeSynonym _ _) = s `unifyTypes` ty
  unifyTypes' (ForAll ident1 ty1) (ForAll ident2 ty2) = do
    sk <- skolemize ident1 ty1
    replaced <- replaceVarWithUnknown ident2 ty2
    sk `unifyTypes` replaced
  unifyTypes' (ForAll ident ty1) ty2 = do
    sk <- skolemize ident ty1
    sk `unifyTypes` ty2
  unifyTypes' ty f@(ForAll _ _) = f `unifyTypes` ty
  unifyTypes' Number Number = return ()
  unifyTypes' String String = return ()
  unifyTypes' Boolean Boolean = return ()
  unifyTypes' (Array s) (Array t) = s `unifyTypes` t
  unifyTypes' (Object row1) (Object row2) = row1 ~~ row2
  unifyTypes' (Function args1 ret1) (Function args2 ret2) = do
    guardWith "Function applied to incorrect number of args" $ length args1 == length args2
    zipWithM_ unifyTypes args1 args2
    ret1 `unifyTypes` ret2
  unifyTypes' (TypeVar v1) (TypeVar v2) | v1 == v2 = return ()
  unifyTypes' (TypeConstructor c1) (TypeConstructor c2) = do
    modulePath <- checkModulePath `fmap` lift get
    guardWith ("Cannot unify " ++ show c1 ++ " with " ++ show c2 ++ ".") (qualify modulePath c1 == qualify modulePath c2)
  unifyTypes' (TypeApp t1 t2) (TypeApp t3 t4) = do
    t1 `unifyTypes` t3
    t2 `unifyTypes` t4
  unifyTypes' (Skolem s1) (Skolem s2) | s1 == s2 = return ()
  unifyTypes' t1 t2 = throwError $ "Cannot unify " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2 ++ "."

isFunction :: Value -> Bool
isFunction (Abs _ _) = True
isFunction (TypedValue untyped _) = isFunction untyped
isFunction _ = False

typeOf :: Maybe Ident -> Value -> Check Type
typeOf name val = do
  (ty, sub, checks) <- runSubst $ case name of
        Just ident | isFunction val ->
          case val of
            TypedValue val ty -> do
              kind <- lift $ kindOf ty
              guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
              ty' <- lift $ replaceAllTypeSynonyms ty
              check (M.singleton ident ty) val ty'
              return ty'
            _ -> do
              me <- fresh
              ty <- infer (M.singleton ident me) val
              ty ~~ me
              return ty
        _ -> infer M.empty val
  escapeCheck checks ty sub
  skolemEscapeCheck ty
  return $ varIfUnknown $ desaturateAllTypeSynonyms $ setifyAll ty

escapeCheck :: [AnyUnifiable] -> Type -> Substitution -> Check ()
escapeCheck checks ty sub =
  let
    visibleUnknowns = nub $ unknowns ty
  in
    forM_ checks $ \check -> case check of
      AnyUnifiable t -> do
        let unsolvedUnknowns = nub . unknowns $ apply sub t
        guardWith "Escape check fails" $ null $ unsolvedUnknowns \\ visibleUnknowns

skolemEscapeCheck :: Type -> Check ()
skolemEscapeCheck ty =
  case something (extQ (mkQ Nothing findSkolems) findRSkolems) ty of
    Nothing -> return ()
    Just _ -> throwError "Skolem variables cannot escape. Consider adding a type signature."
  where
    findSkolems (Skolem _) = return ()
    findSkolems _ = mzero
    findRSkolems (RSkolem _) = return ()
    findRSkolems _ = mzero

setify :: Row -> Row
setify = rowFromList . first (M.toList . M.fromList) . rowToList

setifyAll :: (D.Data d) => d -> d
setifyAll = everywhere (mkT setify)

varIfUnknown :: Type -> Type
varIfUnknown ty = mkForAll (sort . map ((:) 'u' . show) . nub $ unknowns ty) ty

replaceAllTypeVars :: (D.Data d) => [(String, Type)] -> d -> d
replaceAllTypeVars = foldl' (\f (name, ty) -> replaceTypeVars name ty . f) id

replaceTypeVars :: (D.Data d) => String -> Type -> d -> d
replaceTypeVars name t = everywhereBut (mkQ False isShadowed) (mkT replace)
  where
  replace (TypeVar v) | v == name = t
  replace t = t
  isShadowed (ForAll v _) | v == name = True
  isShadowed _ = False

replaceRowVars :: (D.Data d) => String -> Row -> d -> d
replaceRowVars name r = everywhere (mkT replace)
  where
  replace (RowVar v) | v == name = r
  replace t = t

replaceAllVarsWithUnknowns :: Type -> Subst Check Type
replaceAllVarsWithUnknowns (ForAll ident ty) = replaceVarWithUnknown ident ty >>= replaceAllVarsWithUnknowns
replaceAllVarsWithUnknowns ty = return ty

replaceVarWithUnknown :: String -> Type -> Subst Check Type
replaceVarWithUnknown ident ty = do
  tu <- fresh
  ru <- fresh
  return $ replaceRowVars ident ru . replaceTypeVars ident tu $ ty

replaceAllTypeSynonyms :: (D.Data d) => d -> Check d
replaceAllTypeSynonyms d = do
  env <- getEnv
  let syns = map (\((path, name), (args, _)) -> (Qualified path name, length args)) . M.toList $ typeSynonyms env
  either throwError return $ saturateAllTypeSynonyms syns d

desaturateAllTypeSynonyms :: (D.Data d) => d -> d
desaturateAllTypeSynonyms = everywhere (mkT replace)
  where
  replace (SaturatedTypeSynonym name args) = foldl TypeApp (TypeConstructor name) args
  replace t = t

expandAllTypeSynonyms :: Type -> Subst Check Type
expandAllTypeSynonyms (SaturatedTypeSynonym name args) = expandTypeSynonym name args >>= expandAllTypeSynonyms
expandAllTypeSynonyms ty = return ty

expandTypeSynonym :: Qualified ProperName -> [Type] -> Subst Check Type
expandTypeSynonym name args = do
  env <- lift getEnv
  modulePath <- checkModulePath `fmap` lift get
  case M.lookup (qualify modulePath name) (typeSynonyms env) of
    Just (synArgs, body) -> return $ replaceAllTypeVars (zip synArgs args) body
    Nothing -> error "Type synonym was not defined"

ensureNoDuplicateProperties :: [(String, Value)] -> Check ()
ensureNoDuplicateProperties ps = guardWith "Duplicate property names" $ length (nub . map fst $ ps) == length ps

infer :: M.Map Ident Type -> Value -> Subst Check Type
infer m val = rethrow (\e -> "Error inferring type of term " ++ prettyPrintValue val ++ ":\n" ++ e) $ do
  ty <- infer' m val
  escapeCheckLater ty
  return ty

infer' _ (NumericLiteral _) = return Number
infer' _ (StringLiteral _) = return String
infer' _ (BooleanLiteral _) = return Boolean
infer' m (ArrayLiteral vals) = do
  ts <- mapM (infer m) vals
  arr <- fresh
  forM_ ts $ \t -> arr ~~ Array t
  return arr
infer' m (Unary op val) = do
  t <- infer m val
  inferUnary op t
infer' m (Binary op left right) = do
  t1 <- infer m left
  t2 <- infer m right
  inferBinary op t1 t2
infer' m (ObjectLiteral ps) = do
  lift $ ensureNoDuplicateProperties ps
  ts <- mapM (infer m . snd) ps
  let fields = zipWith (\(name, _) t -> (name, t)) ps ts
  return $ Object $ rowFromList (fields, REmpty)
infer' m (ObjectUpdate o ps) = do
  lift $ ensureNoDuplicateProperties ps
  row <- fresh
  newTys <- zipWith (\(name, _) t -> (name, t)) ps <$> mapM (infer m . snd) ps
  oldTys <- zip (map fst ps) <$> replicateM (length ps) fresh
  check m o $ Object $ rowFromList (oldTys, row)
  return $ Object $ rowFromList (newTys, row)
infer' m (Indexer index val) = do
  el <- fresh
  check m index Number
  check m val (Array el)
  return el
infer' m (Accessor prop val) = do
  obj <- infer m val
  propTy <- inferProperty obj prop
  case propTy of
    Nothing -> do
      field <- fresh
      rest <- fresh
      obj `subsumes` Object (RCons prop field rest)
      return field
    Just ty -> return ty
infer' m (Abs args ret) = do
  ts <- replicateM (length args) fresh
  let m' = m `M.union` M.fromList (zip args ts)
  body <- infer m' ret
  return $ Function ts body
infer' m app@(App _ _) = do
  let (f, argss) = unfoldApplication app
  ft <- infer m f
  ret <- fresh
  checkFunctionApplications m ft argss ret
  return ret
infer' m (Var var@(Qualified mp name)) = do
  case mp of
    ModulePath [] ->
      case M.lookup name m of
        Just ty -> lift $ replaceAllTypeSynonyms ty
        Nothing -> lookupGlobal
    _ -> lookupGlobal
  where
  lookupGlobal = do
    env <- lift getEnv
    modulePath <- checkModulePath `fmap` lift get
    case M.lookup (qualify modulePath var) (names env) of
      Nothing -> throwError $ show var ++ " is undefined"
      Just (ty, _) -> lift $ replaceAllTypeSynonyms ty
infer' m (Block ss) = do
  ret <- fresh
  (allCodePathsReturn, _) <- checkBlock m M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
  return ret
infer' m (Constructor c) = do
  env <- lift getEnv
  modulePath <- checkModulePath `fmap` lift get
  case M.lookup (qualify modulePath c) (dataConstructors env) of
    Nothing -> throwError $ "Constructor " ++ show c ++ " is undefined"
    Just ty -> lift $ replaceAllTypeSynonyms ty
infer' m (Case val binders) = do
  t1 <- infer m val
  ret <- fresh
  checkBinders m t1 ret binders
  return ret
infer' m (IfThenElse cond th el) = do
  check m cond Boolean
  t2 <- infer m th
  t3 <- infer m el
  t2 ~~ t3
  return t2
infer' m (TypedValue val ty) = do
  kind <- lift $ kindOf ty
  guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty' <- lift $ replaceAllTypeSynonyms ty
  check m val ty'
  return ty'

inferProperty :: Type -> String -> Subst Check (Maybe Type)
inferProperty (Object row) prop = do
  let (props, _) = rowToList row
  return $ lookup prop props
inferProperty (SaturatedTypeSynonym name args) prop = do
  replaced <- expandTypeSynonym name args
  inferProperty replaced prop
inferProperty (ForAll ident ty) prop = do
  replaced <- replaceVarWithUnknown ident ty
  inferProperty replaced prop
inferProperty _ prop = return Nothing

inferUnary :: UnaryOperator -> Type -> Subst Check Type
inferUnary op val =
  case fromMaybe (error "Invalid operator") $ lookup op unaryOps of
    (valTy, resTy) -> do
      val ~~ valTy
      return resTy

checkUnary :: M.Map Ident Type -> UnaryOperator -> Value -> Type -> Subst Check ()
checkUnary m op val res =
  case fromMaybe (error "Invalid operator") $ lookup op unaryOps of
    (valTy, resTy) -> do
      res ~~ resTy
      check m val valTy

unaryOps :: [(UnaryOperator, (Type, Type))]
unaryOps = [ (Negate, (Number, Number))
           , (Not, (Boolean, Boolean))
           , (BitwiseNot, (Number, Number))
           ]

inferBinary :: BinaryOperator -> Type -> Type -> Subst Check Type
inferBinary op left right | isEqualityTest op = do
  left ~~ right
  return Boolean
inferBinary op left right =
  case fromMaybe (error "Invalid operator") $ lookup op binaryOps of
    (valTy, resTy) -> do
      left ~~ valTy
      right ~~ valTy
      return resTy

checkBinary :: M.Map Ident Type -> BinaryOperator -> Value -> Value -> Type -> Subst Check ()
checkBinary m op left right res | isEqualityTest op = do
  res ~~ Boolean
  t1 <- infer m left
  t2 <- infer m right
  t1 ~~ t2
checkBinary m op left right res =
  case fromMaybe (error "Invalid operator") $ lookup op binaryOps of
    (valTy, resTy) -> do
      res ~~ resTy
      check m left valTy
      check m right valTy

isEqualityTest :: BinaryOperator -> Bool
isEqualityTest EqualTo = True
isEqualityTest NotEqualTo = True
isEqualityTest _ = False

binaryOps :: [(BinaryOperator, (Type, Type))]
binaryOps = [ (Add, (Number, Number))
            , (Subtract, (Number, Number))
            , (Multiply, (Number, Number))
            , (Divide, (Number, Number))
            , (Modulus, (Number, Number))
            , (BitwiseAnd, (Number, Number))
            , (BitwiseOr, (Number, Number))
            , (BitwiseXor, (Number, Number))
            , (ShiftRight, (Number, Number))
            , (ZeroFillShiftRight, (Number, Number))
            , (And, (Boolean, Boolean))
            , (Or, (Boolean, Boolean))
            , (Concat, (String, String))
            , (Modulus, (Number, Number))
            , (LessThan, (Number, Boolean))
            , (LessThanOrEqualTo, (Number, Boolean))
            , (GreaterThan, (Number, Boolean))
            , (GreaterThanOrEqualTo, (Number, Boolean))
            ]

inferBinder :: Type -> Binder -> Subst Check (M.Map Ident Type)
inferBinder _ NullBinder = return M.empty
inferBinder val (StringBinder _) = val ~~ String >> return M.empty
inferBinder val (NumberBinder _) = val ~~ Number >> return M.empty
inferBinder val (BooleanBinder _) = val ~~ Boolean >> return M.empty
inferBinder val (VarBinder name) = return $ M.singleton name val
inferBinder val (NullaryBinder ctor) = do
  env <- lift getEnv
  modulePath <- checkModulePath `fmap` lift get
  case M.lookup (qualify modulePath ctor) (dataConstructors env) of
    Just ty -> do
      ty `subsumes` val
      return M.empty
    _ -> throwError $ "Constructor " ++ show ctor ++ " is not defined"
inferBinder val (UnaryBinder ctor binder) = do
  env <- lift getEnv
  modulePath <- checkModulePath `fmap` lift get
  case M.lookup (qualify modulePath ctor) (dataConstructors env) of
    Just ty -> do
      Function [obj] ret <- replaceAllVarsWithUnknowns ty
      val `subsumes` ret
      inferBinder obj binder
    _ -> throwError $ "Constructor " ++ show ctor ++ " is not defined"
inferBinder val (ObjectBinder props) = do
  row <- fresh
  rest <- fresh
  m1 <- inferRowProperties row rest props
  val ~~ Object row
  return m1
  where
  inferRowProperties :: Row -> Row -> [(String, Binder)] -> Subst Check (M.Map Ident Type)
  inferRowProperties nrow row [] = nrow ~~ row >> return M.empty
  inferRowProperties nrow row ((name, binder):binders) = do
    propTy <- fresh
    m1 <- inferBinder propTy binder
    m2 <- inferRowProperties nrow (RCons name propTy row) binders
    return $ m1 `M.union` m2
inferBinder val (ArrayBinder binders rest) = do
  el <- fresh
  m1 <- M.unions <$> mapM (inferBinder el) binders
  val ~~ Array el
  case rest of
    Nothing -> return m1
    Just binder -> do
      m2 <- inferBinder val binder
      return $ m1 `M.union` m2
inferBinder val (NamedBinder name binder) = do
  m <- inferBinder val binder
  return $ M.insert name val m

inferGuardedBinder :: M.Map Ident Type -> Type -> Binder -> Subst Check (M.Map Ident Type)
inferGuardedBinder m val (GuardedBinder cond binder) = do
  m1 <- inferBinder val binder
  check (m1 `M.union` m) cond Boolean
  return m1
inferGuardedBinder m val b = inferBinder val b

checkBinders :: M.Map Ident Type -> Type -> Type -> [(Binder, Value)] -> Subst Check ()
checkBinders _ _ _ [] = return ()
checkBinders m nval ret ((binder, val):bs) = do
  m1 <- inferGuardedBinder m nval binder
  check (m1 `M.union` m) val ret
  checkBinders m nval ret bs

assignVariable :: Ident -> M.Map Ident Type -> Subst Check ()
assignVariable name m =
  case M.lookup name m of
    Nothing -> return ()
    Just _ -> throwError $ "Variable with name " ++ show name ++ " already exists."

checkStatement :: M.Map Ident Type -> M.Map Ident Type -> Type -> Statement -> Subst Check (Bool, M.Map Ident Type)
checkStatement m mass ret (VariableIntroduction name val) = do
  assignVariable name (m `M.union` mass)
  t <- infer m val
  return (False, M.insert name t mass)
checkStatement m mass ret (Assignment ident val) = do
  t <- infer m val
  case M.lookup ident mass of
    Nothing -> throwError $ "No local variable with name " ++ show ident
    Just ty -> do t ~~ ty
                  return (False, mass)
checkStatement m mass ret (While val inner) = do
  check m val Boolean
  (allCodePathsReturn, _) <- checkBlock m mass ret inner
  return (allCodePathsReturn, mass)
checkStatement m mass ret (If ifst) = do
  allCodePathsReturn <- checkIfStatement m mass ret ifst
  return (allCodePathsReturn, mass)
checkStatement m mass ret (For ident start end inner) = do
  assignVariable ident (m `M.union` mass)
  check (m `M.union` mass) start Number
  check (m `M.union` mass) end Number
  let mass1 = M.insert ident Number mass
  (allCodePathsReturn, _) <- checkBlock (m `M.union` mass1) mass1 ret inner
  return (allCodePathsReturn, mass)
checkStatement m mass ret (ForEach ident vals inner) = do
  assignVariable ident (m `M.union` mass)
  val <- fresh
  check (m `M.union` mass) vals (Array val)
  let mass1 = M.insert ident val mass
  (allCodePathsReturn, _) <- checkBlock (m `M.union` mass1) mass1 ret inner
  guardWith "Cannot return from within a foreach block" $ not allCodePathsReturn
  return (False, mass)
checkStatement m mass ret (Return val) = do
  check (m `M.union` mass) val ret
  return (True, mass)

checkIfStatement :: M.Map Ident Type -> M.Map Ident Type -> Type -> IfStatement -> Subst Check Bool
checkIfStatement m mass ret (IfStatement val thens Nothing) = do
  check m val Boolean
  _ <- checkBlock m mass ret thens
  return False
checkIfStatement m mass ret (IfStatement val thens (Just elses)) = do
  check m val Boolean
  (allCodePathsReturn1, _) <- checkBlock m mass ret thens
  allCodePathsReturn2 <- checkElseStatement m mass ret elses
  return $ allCodePathsReturn1 && allCodePathsReturn2

checkElseStatement :: M.Map Ident Type -> M.Map Ident Type -> Type -> ElseStatement -> Subst Check Bool
checkElseStatement m mass ret (Else elses) = fst <$> checkBlock m mass ret elses
checkElseStatement m mass ret (ElseIf ifst) = checkIfStatement m mass ret ifst

checkBlock :: M.Map Ident Type -> M.Map Ident Type -> Type -> [Statement] -> Subst Check (Bool, M.Map Ident Type)
checkBlock _ mass _ [] = return (False, mass)
checkBlock m mass ret (s:ss) = do
  (b1, mass1) <- checkStatement (m `M.union` mass) mass ret s
  case (b1, ss) of
    (True, []) -> return (True, mass1)
    (True, _) -> throwError "Unreachable code"
    (False, ss) -> do
      (b2, mass2) <- checkBlock m mass1 ret ss
      return (b2, mass2)

skolemize :: String -> Type -> Subst Check Type
skolemize ident ty = do
  tsk <- Skolem <$> fresh'
  rsk <- RSkolem <$> fresh'
  return $ replaceRowVars ident rsk $ replaceTypeVars ident tsk ty

check :: M.Map Ident Type -> Value -> Type -> Subst Check ()
check m val ty = rethrow errorMessage $ check' m val ty
  where
  errorMessage msg =
    "Error checking type of term " ++
    prettyPrintValue val ++
    " against type " ++
    prettyPrintType ty ++
    ":\n" ++
    msg

check' :: M.Map Ident Type -> Value -> Type -> Subst Check ()
check' m val (ForAll idents ty) = do
  sk <- skolemize idents ty
  check m val sk
check' m val u@(TUnknown _) = do
  ty <- infer m val
  -- Don't unify an unknown with an inferred polytype
  ty' <- replaceAllVarsWithUnknowns ty
  ty' ~~ u
check' m (NumericLiteral _) Number = return ()
check' m (StringLiteral _) String = return ()
check' m (BooleanLiteral _) Boolean = return ()
check' m (Unary op val) ty = checkUnary m op val ty
check' m (Binary op left right) ty = checkBinary m op left right ty
check' m (ArrayLiteral vals) (Array ty) = forM_ vals (\val -> check m val ty)
check' m (Indexer index vals) ty = check m index Number >> check m vals (Array ty)
check' m (Abs args ret) (Function argTys retTy) = do
  guardWith "Incorrect number of function arguments" (length args == length argTys)
  let bindings = M.fromList (zip args argTys)
  check (bindings `M.union` m) ret retTy
check' m app@(App _ _) ret = do
  let (f, argss) = unfoldApplication app
  ft <- infer m f
  checkFunctionApplications m ft argss ret
check' m v@(Var var@(Qualified mp name)) ty = do
  case mp of
    ModulePath [] ->
      case M.lookup name m of
        Just ty1 -> do
          repl <- lift $ replaceAllTypeSynonyms ty1
          repl `subsumes` ty
        Nothing -> lookupGlobal
    _ -> lookupGlobal
  where
  lookupGlobal = do
    env <- lift getEnv
    modulePath <- checkModulePath `fmap` lift get
    case M.lookup (qualify modulePath var) (names env) of
      Nothing -> throwError $ show var ++ " is undefined"
      Just (ty1, _) -> do
        repl <- lift $ replaceAllTypeSynonyms ty1
        repl `subsumes` ty
check' m (TypedValue val ty1) ty2 = do
  kind <- lift $ kindOf ty1
  guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty1 `subsumes` ty2
  check m val ty1
check' m (Case val binders) ret = do
  t1 <- infer m val
  checkBinders m t1 ret binders
check' m (IfThenElse cond th el) ty = do
  check m cond Boolean
  check m th ty
  check m el ty
check' m (ObjectLiteral ps) (Object row) = do
  lift $ ensureNoDuplicateProperties ps
  checkProperties m ps row False
check' m (ObjectUpdate obj ps) (Object row) = do
  lift $ ensureNoDuplicateProperties ps
  us <- zip (map fst ps) <$> replicateM (length ps) fresh
  let (propsToCheck, rest) = rowToList row
      propsToRemove = map fst ps
      remainingProps = filter (\(p, _) -> p `notElem` propsToRemove) propsToCheck
  check m obj (Object (rowFromList (us ++ remainingProps, rest)))
  checkProperties m ps row True
check' m (Accessor prop val) ty = do
  rest <- fresh
  check m val (Object (RCons prop ty rest))
check' m (Block ss) ret = do
  (allCodePathsReturn, _) <- checkBlock m M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
check' m (Constructor c) ty = do
  env <- lift getEnv
  modulePath <- checkModulePath `fmap` lift get
  case M.lookup (qualify modulePath c) (dataConstructors env) of
    Nothing -> throwError $ "Constructor " ++ show c ++ " is undefined"
    Just ty1 -> do
      repl <- lift $ replaceAllTypeSynonyms ty1
      repl `subsumes` ty
check' m val (SaturatedTypeSynonym name args) = do
  ty <- expandTypeSynonym name args
  check m val ty
check' _ val ty = throwError $ prettyPrintValue val ++ " does not have type " ++ prettyPrintType ty

checkProperties :: M.Map Ident Type -> [(String, Value)] -> Row -> Bool -> Subst Check ()
checkProperties m ps row lax = let (ts, r') = rowToList row in go ps ts r' where
  go [] [] REmpty = return ()
  go [] [] u@(RUnknown _) = u ~~ REmpty
  go [] [] (RSkolem _) | lax = return ()
  go [] ((p, _): _) _ | lax = return ()
                      | otherwise = throwError $ prettyPrintValue (ObjectLiteral ps) ++ " does not have property " ++ p
  go ((p,_):_) [] REmpty = throwError $ "Property " ++ p ++ " is not present in closed object type " ++ prettyPrintRow row
  go ((p,v):ps) [] u@(RUnknown _) = do
    ty <- infer m v
    rest <- fresh
    u ~~ RCons p ty rest
    go ps [] rest
  go ((p,v):ps) ts r =
    case lookup p ts of
      Nothing -> do
        ty <- infer m v
        rest <- fresh
        r ~~ RCons p ty rest
        go ps ts rest
      Just ty -> do
        check m v ty
        go ps (delete (p, ty) ts) r
  go _ _ _ = throwError $ prettyPrintValue (ObjectLiteral ps) ++ " does not have type " ++ prettyPrintType (Object row)

unfoldApplication :: Value -> (Value, [[Value]])
unfoldApplication = go []
  where
  go argss (App f args) = go (args:argss) f
  go argss f = (f, argss)

checkFunctionApplications :: M.Map Ident Type -> Type -> [[Value]] -> Type -> Subst Check ()
checkFunctionApplications _ _ [] _ = error "Nullary function application"
checkFunctionApplications m fnTy [args] ret = checkFunctionApplication m fnTy args ret
checkFunctionApplications m fnTy (args:argss) ret = do
  f <- fresh
  checkFunctionApplication m fnTy args f
  checkFunctionApplications m f argss ret

checkFunctionApplication :: M.Map Ident Type -> Type -> [Value] -> Type -> Subst Check ()
checkFunctionApplication m fnTy args ret = rethrow errorMessage $ checkFunctionApplication' m fnTy args ret
  where
  errorMessage msg = "Error applying function of type "
    ++ prettyPrintType fnTy
    ++ " to arguments " ++ intercalate ", " (map prettyPrintValue args)
    ++ ", expecting value of type "
    ++ prettyPrintType ret ++ ":\n" ++ msg

checkFunctionApplication' :: M.Map Ident Type -> Type -> [Value] -> Type -> Subst Check ()
checkFunctionApplication' m (Function argTys retTy) args ret = do
  guardWith "Incorrect number of function arguments" (length args == length argTys)
  zipWithM (check m) args argTys
  retTy `subsumes` ret
checkFunctionApplication' m (ForAll ident ty) args ret = do
  replaced <- replaceVarWithUnknown ident ty
  checkFunctionApplication m replaced args ret
checkFunctionApplication' m u@(TUnknown _) args ret = do
  tyArgs <- mapM (\arg -> infer m arg >>= replaceAllVarsWithUnknowns) args
  u ~~ Function tyArgs ret
checkFunctionApplication' m (SaturatedTypeSynonym name tyArgs) args ret = do
  ty <- expandTypeSynonym name tyArgs
  checkFunctionApplication' m ty args ret
checkFunctionApplication' _ fnTy args ret = throwError $ "Cannot apply function of type "
  ++ prettyPrintType fnTy
  ++ " to arguments " ++ intercalate ", " (map prettyPrintValue args)
  ++ ". Expecting value of type " ++ prettyPrintType ret ++ "."

subsumes :: Type -> Type -> Subst Check ()
subsumes (ForAll ident ty1) ty2 = do
  replaced <- replaceVarWithUnknown ident ty1
  replaced `subsumes` ty2
subsumes (Function args1 ret1) (Function args2 ret2) = do
  zipWithM subsumes args2 args1
  ret1 `subsumes` ret2
subsumes ty1 ty2 = ty1 ~~ ty2

