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

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module Language.PureScript.TypeChecker.Types (
    typeOf
) where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Data as D
import Data.Generics
       (mkT, something, everywhere, everywhereBut, mkQ, extQ)

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
import Control.Arrow (Arrow(..))

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
  unknowns (ForAll _ ty) = unknowns ty
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
      unifyRows :: [(String, Type)] -> Row -> [(String, Type)] -> Row -> Subst ()
      unifyRows [] (RUnknown u) sd r = replace u (rowFromList (sd, r))
      unifyRows sd r [] (RUnknown u) = replace u (rowFromList (sd, r))
      unifyRows ((name, ty):row) r others u@(RUnknown un) = do
        occursCheck un ty
        forM row $ \(_, t) -> occursCheck un t
        u' <- fresh
        u ~~ RCons name ty u'
        unifyRows row r others u'
      unifyRows [] REmpty [] REmpty = return ()
      unifyRows [] (RowVar v1) [] (RowVar v2) | v1 == v2 = return ()
      unifyRows [] (RSkolem s1) [] (RSkolem s2) | s1 == s2 = return ()
      unifyRows sd3 r3 sd4 r4 = throwError $ "Cannot unify " ++ prettyPrintRow (rowFromList (sd3, r3)) ++ " with " ++ prettyPrintRow (rowFromList (sd4, r4)) ++ "."
  apply s (RUnknown u) = runSubstitution s u
  apply s (RCons name ty r) = RCons name (apply s ty) (apply s r)
  apply _ r = r
  unknowns (RUnknown (Unknown u)) = [u]
  unknowns (RCons _ ty r) = unknowns ty ++ unknowns r
  unknowns _ = []

unifyTypes :: Type -> Type -> Subst ()
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
    modulePath <- checkModulePath `fmap` get
    guardWith ("Cannot unify " ++ show c1 ++ " with " ++ show c2 ++ ".") (qualify modulePath c1 == qualify modulePath c2)
  unifyTypes' (TypeApp t3 t4) (TypeApp t5 t6) = do
    t3 `unifyTypes` t5
    t4 `unifyTypes` t6
  unifyTypes' (Skolem s1) (Skolem s2) | s1 == s2 = return ()
  unifyTypes' t3 t4 = throwError $ "Cannot unify " ++ prettyPrintType t3 ++ " with " ++ prettyPrintType t4 ++ "."

isFunction :: Value -> Bool
isFunction (Abs _ _) = True
isFunction (TypedValue untyped _) = isFunction untyped
isFunction _ = False

typeOf :: Maybe Ident -> Value -> Check Type
typeOf name val = do
  (ty, sub, checks) <- runSubst $ case name of
        Just ident | isFunction val ->
          case val of
            TypedValue value ty -> do
              kind <- liftCheck $ kindOf ty
              guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
              ty' <- replaceAllTypeSynonyms ty
              modulePath <- checkModulePath <$> get
              bindNames (M.singleton (modulePath, ident) (ty, LocalVariable)) $ check value ty'
              return ty'
            _ -> do
              me <- fresh
              modulePath <- checkModulePath <$> get
              ty <- bindNames (M.singleton (modulePath, ident) (me, LocalVariable)) $ infer val
              ty ~~ me
              return ty
        _ -> infer val
  escapeCheck checks ty sub
  skolemEscapeCheck ty
  return $ varIfUnknown $ desaturateAllTypeSynonyms $ setifyAll ty

escapeCheck :: [AnyUnifiable] -> Type -> Substitution -> Check ()
escapeCheck checks ty sub =
  let
    visibleUnknowns = nub $ unknowns ty
  in
    forM_ checks $ \c -> case c of
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
varIfUnknown ty =
  let unks = nub $ unknowns ty
      toName = (:) 't' . show
      ty' = everywhere (mkT rowToVar) . everywhere (mkT typeToVar) $ ty
      typeToVar :: Type -> Type
      typeToVar (TUnknown (Unknown u)) = TypeVar (toName u)
      typeToVar t = t
      rowToVar :: Row -> Row
      rowToVar (RUnknown (Unknown u)) = RowVar (toName u)
      rowToVar t = t
  in mkForAll (sort . map toName $ unks) ty'

replaceAllTypeVars :: (D.Data d) => [(String, Type)] -> d -> d
replaceAllTypeVars = foldl' (\f (name, ty) -> replaceTypeVars name ty . f) id

replaceTypeVars :: (D.Data d) => String -> Type -> d -> d
replaceTypeVars name t = everywhereBut (mkQ False isShadowed) (mkT replaceTypeVar)
  where
  replaceTypeVar (TypeVar v) | v == name = t
  replaceTypeVar other = other
  isShadowed (ForAll v _) | v == name = True
  isShadowed _ = False

replaceRowVars :: (D.Data d) => String -> Row -> d -> d
replaceRowVars name r = everywhere (mkT replaceRowVar)
  where
  replaceRowVar (RowVar v) | v == name = r
  replaceRowVar other = other

replaceAllVarsWithUnknowns :: Type -> Subst Type
replaceAllVarsWithUnknowns (ForAll ident ty) = replaceVarWithUnknown ident ty >>= replaceAllVarsWithUnknowns
replaceAllVarsWithUnknowns ty = return ty

replaceVarWithUnknown :: String -> Type -> Subst Type
replaceVarWithUnknown ident ty = do
  tu <- fresh
  ru <- fresh
  return $ replaceRowVars ident ru . replaceTypeVars ident tu $ ty

replaceAllTypeSynonyms :: (Functor m, MonadState CheckState m, MonadError String m) => (D.Data d) => d -> m d
replaceAllTypeSynonyms d = do
  env <- getEnv
  let syns = map (\((path, name), (args, _)) -> (Qualified path name, length args)) . M.toList $ typeSynonyms env
  either throwError return $ saturateAllTypeSynonyms syns d

desaturateAllTypeSynonyms :: (D.Data d) => d -> d
desaturateAllTypeSynonyms = everywhere (mkT replaceSaturatedTypeSynonym)
  where
  replaceSaturatedTypeSynonym (SaturatedTypeSynonym name args) = foldl TypeApp (TypeConstructor name) args
  replaceSaturatedTypeSynonym t = t

expandTypeSynonym :: Qualified ProperName -> [Type] -> Subst Type
expandTypeSynonym name args = do
  env <- getEnv
  modulePath <- checkModulePath `fmap` get
  case M.lookup (qualify modulePath name) (typeSynonyms env) of
    Just (synArgs, body) -> return $ replaceAllTypeVars (zip synArgs args) body
    Nothing -> error "Type synonym was not defined"

ensureNoDuplicateProperties :: (MonadError String m) => [(String, Value)] -> m ()
ensureNoDuplicateProperties ps = guardWith "Duplicate property names" $ length (nub . map fst $ ps) == length ps

infer :: Value -> Subst Type
infer val = rethrow (\e -> "Error inferring type of term " ++ prettyPrintValue val ++ ":\n" ++ e) $ do
  ty <- infer' val
  escapeCheckLater ty
  return ty

infer' :: Value -> Subst Type
infer' (NumericLiteral _) = return Number
infer' (StringLiteral _) = return String
infer' (BooleanLiteral _) = return Boolean
infer' (ArrayLiteral vals) = do
  ts <- mapM (infer) vals
  els <- fresh
  forM_ ts $ \t -> els ~~ Array t
  return els
infer' (Unary op val) = do
  t <- infer val
  inferUnary op t
infer' (Binary op left right) = do
  t1 <- infer left
  t2 <- infer right
  inferBinary op t1 t2
infer' (ObjectLiteral ps) = do
  ensureNoDuplicateProperties ps
  ts <- mapM (infer . snd) ps
  let fields = zipWith (\(name, _) t -> (name, t)) ps ts
  return $ Object $ rowFromList (fields, REmpty)
infer' (ObjectUpdate o ps) = do
  ensureNoDuplicateProperties ps
  row <- fresh
  newTys <- zipWith (\(name, _) t -> (name, t)) ps <$> mapM (infer . snd) ps
  oldTys <- zip (map fst ps) <$> replicateM (length ps) fresh
  check o $ Object $ rowFromList (oldTys, row)
  return $ Object $ rowFromList (newTys, row)
infer' (Indexer index val) = do
  el <- fresh
  check index Number
  check val (Array el)
  return el
infer' (Accessor prop val) = do
  obj <- infer val
  propTy <- inferProperty obj prop
  case propTy of
    Nothing -> do
      field <- fresh
      rest <- fresh
      obj `subsumes` Object (RCons prop field rest)
      return field
    Just ty -> return ty
infer' (Abs args ret) = do
  ts <- replicateM (length args) fresh
  bindLocalVariables (zip args ts) $ do
    body <- infer' ret
    return $ Function ts body
infer' app@(App _ _) = do
  let (f, argss) = unfoldApplication app
  ft <- infer f
  ret <- fresh
  checkFunctionApplications ft argss ret
  return ret
infer' (Var var) = do
  ty <- lookupVariable var
  replaceAllTypeSynonyms ty
infer' (Block ss) = do
  ret <- fresh
  (allCodePathsReturn, _) <- checkBlock M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
  return ret
infer' (Constructor c) = do
  env <- getEnv
  modulePath <- checkModulePath `fmap` get
  case M.lookup (qualify modulePath c) (dataConstructors env) of
    Nothing -> throwError $ "Constructor " ++ show c ++ " is undefined"
    Just ty -> replaceAllTypeSynonyms ty
infer' (Case vals binders) = do
  ts <- mapM infer vals
  ret <- fresh
  checkBinders ts ret binders
  return ret
infer' (IfThenElse cond th el) = do
  check cond Boolean
  t2 <- infer th
  t3 <- infer el
  t2 ~~ t3
  return t2
infer' (TypedValue val ty) = do
  kind <- liftCheck $ kindOf ty
  guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty' <- replaceAllTypeSynonyms ty
  check val ty'
  return ty'
infer' _ = error "Invalid argument to infer"

inferProperty :: Type -> String -> Subst (Maybe Type)
inferProperty (Object row) prop = do
  let (props, _) = rowToList row
  return $ lookup prop props
inferProperty (SaturatedTypeSynonym name args) prop = do
  replaced <- expandTypeSynonym name args
  inferProperty replaced prop
inferProperty (ForAll ident ty) prop = do
  replaced <- replaceVarWithUnknown ident ty
  inferProperty replaced prop
inferProperty _ _ = return Nothing

inferUnary :: UnaryOperator -> Type -> Subst Type
inferUnary op val =
  case fromMaybe (error "Invalid operator") $ lookup op unaryOps of
    (valTy, resTy) -> do
      val ~~ valTy
      return resTy

checkUnary :: UnaryOperator -> Value -> Type -> Subst ()
checkUnary op val res =
  case fromMaybe (error "Invalid operator") $ lookup op unaryOps of
    (valTy, resTy) -> do
      res ~~ resTy
      check val valTy

unaryOps :: [(UnaryOperator, (Type, Type))]
unaryOps = [ (Negate, (Number, Number))
           , (Not, (Boolean, Boolean))
           , (BitwiseNot, (Number, Number))
           ]

inferBinary :: BinaryOperator -> Type -> Type -> Subst Type
inferBinary op left right | isEqualityTest op = do
  left ~~ right
  return Boolean
inferBinary op left right =
  case fromMaybe (error "Invalid operator") $ lookup op binaryOps of
    (valTy, resTy) -> do
      left ~~ valTy
      right ~~ valTy
      return resTy

checkBinary :: BinaryOperator -> Value -> Value -> Type -> Subst ()
checkBinary op left right res | isEqualityTest op = do
  res ~~ Boolean
  t1 <- infer left
  t2 <- infer right
  t1 ~~ t2
checkBinary op left right res =
  case fromMaybe (error "Invalid operator") $ lookup op binaryOps of
    (valTy, resTy) -> do
      res ~~ resTy
      check left valTy
      check right valTy

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

inferBinder :: Type -> Binder -> Subst (M.Map Ident Type)
inferBinder _ NullBinder = return M.empty
inferBinder val (StringBinder _) = val ~~ String >> return M.empty
inferBinder val (NumberBinder _) = val ~~ Number >> return M.empty
inferBinder val (BooleanBinder _) = val ~~ Boolean >> return M.empty
inferBinder val (VarBinder name) = return $ M.singleton name val
inferBinder val (NullaryBinder ctor) = do
  env <- getEnv
  modulePath <- checkModulePath <$> get
  case M.lookup (qualify modulePath ctor) (dataConstructors env) of
    Just ty -> do
      ty `subsumes` val
      return M.empty
    _ -> throwError $ "Constructor " ++ show ctor ++ " is not defined"
inferBinder val (UnaryBinder ctor binder) = do
  env <- getEnv
  modulePath <- checkModulePath <$> get
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
  inferRowProperties :: Row -> Row -> [(String, Binder)] -> Subst (M.Map Ident Type)
  inferRowProperties nrow row [] = nrow ~~ row >> return M.empty
  inferRowProperties nrow row ((name, binder):binders) = do
    propTy <- fresh
    m1 <- inferBinder propTy binder
    m2 <- inferRowProperties nrow (RCons name propTy row) binders
    return $ m1 `M.union` m2
inferBinder val (ArrayBinder binders) = do
  el <- fresh
  m1 <- M.unions <$> mapM (inferBinder el) binders
  val ~~ Array el
  return m1
inferBinder val (ConsBinder headBinder tailBinder) = do
  el <- fresh
  m1 <- inferBinder el headBinder
  m2 <- inferBinder val tailBinder
  val ~~ Array el
  return $ m1 `M.union` m2
inferBinder val (NamedBinder name binder) = do
  m <- inferBinder val binder
  return $ M.insert name val m

checkBinders :: [Type] -> Type -> [([Binder], Maybe Guard, Value)] -> Subst ()
checkBinders _ _ [] = return ()
checkBinders nvals ret ((binders, grd, val):bs) = do
  m1 <- M.unions <$> zipWithM inferBinder nvals binders
  bindLocalVariables (M.toList m1) $ do
    check val ret
    case grd of
      Nothing -> return ()
      Just g -> check g Boolean
  checkBinders nvals ret bs

assignVariable :: Ident -> Subst ()
assignVariable name = do
  env <- checkEnv <$> get
  modulePath <- checkModulePath <$> get
  case M.lookup (modulePath, name) (names env) of
    Just (_, LocalVariable) -> throwError $ "Variable with name " ++ show name ++ " already exists."
    _ -> return ()

checkStatement :: M.Map Ident Type -> Type -> Statement -> Subst (Bool, M.Map Ident Type)
checkStatement mass _ (VariableIntroduction name val) = do
  assignVariable name
  t <- infer val
  return (False, M.insert name t mass)
checkStatement mass _ (Assignment ident val) = do
  t <- infer val
  case M.lookup ident mass of
    Nothing -> throwError $ "No local variable with name " ++ show ident
    Just ty -> do t ~~ ty
                  return (False, mass)
checkStatement mass ret (While val inner) = do
  check val Boolean
  (allCodePathsReturn, _) <- checkBlock mass ret inner
  return (allCodePathsReturn, mass)
checkStatement mass ret (If ifst) = do
  allCodePathsReturn <- checkIfStatement mass ret ifst
  return (allCodePathsReturn, mass)
checkStatement mass ret (For ident start end inner) = do
  assignVariable ident
  check start Number
  check end Number
  (allCodePathsReturn, _) <- bindLocalVariables [(ident, Number)] $ checkBlock mass ret inner
  return (allCodePathsReturn, mass)
checkStatement mass ret (ForEach ident vals inner) = do
  assignVariable ident
  val <- fresh
  check vals (Array val)
  (allCodePathsReturn, _) <- bindLocalVariables [(ident, val)] $ checkBlock mass ret inner
  guardWith "Cannot return from within a foreach block" $ not allCodePathsReturn
  return (False, mass)
checkStatement mass _ (ValueStatement val) = do
  check val unit
  return (False, mass)
checkStatement mass ret (Return val) = do
  check val ret
  return (True, mass)

checkIfStatement :: M.Map Ident Type -> Type -> IfStatement -> Subst Bool
checkIfStatement mass ret (IfStatement val thens Nothing) = do
  check val Boolean
  _ <- checkBlock mass ret thens
  return False
checkIfStatement mass ret (IfStatement val thens (Just elses)) = do
  check val Boolean
  (allCodePathsReturn1, _) <- checkBlock mass ret thens
  allCodePathsReturn2 <- checkElseStatement mass ret elses
  return $ allCodePathsReturn1 && allCodePathsReturn2

checkElseStatement :: M.Map Ident Type -> Type -> ElseStatement -> Subst Bool
checkElseStatement mass ret (Else elses) = fst <$> checkBlock mass ret elses
checkElseStatement mass ret (ElseIf ifst) = checkIfStatement mass ret ifst

checkBlock :: M.Map Ident Type -> Type -> [Statement] -> Subst (Bool, M.Map Ident Type)
checkBlock mass _ [] = return (False, mass)
checkBlock mass ret (s:ss) = do
  (b1, mass1) <- checkStatement mass ret s
  bindLocalVariables (M.toList mass1) $ case (b1, ss) of
    (True, []) -> return (True, mass1)
    (True, _) -> throwError "Unreachable code"
    (False, ss') -> checkBlock mass1 ret ss'

skolemize :: String -> Type -> Subst Type
skolemize ident ty = do
  tsk <- Skolem <$> fresh'
  rsk <- RSkolem <$> fresh'
  return $ replaceRowVars ident rsk $ replaceTypeVars ident tsk ty

check :: Value -> Type -> Subst ()
check val ty = rethrow errorMessage $ check' val ty
  where
  errorMessage msg =
    "Error checking type of term " ++
    prettyPrintValue val ++
    " against type " ++
    prettyPrintType ty ++
    ":\n" ++
    msg

check' :: Value -> Type -> Subst ()
check' val (ForAll idents ty) = do
  sk <- skolemize idents ty
  check val sk
check' val u@(TUnknown _) = do
  ty <- infer val
  -- Don't unify an unknown with an inferred polytype
  ty' <- replaceAllVarsWithUnknowns ty
  ty' ~~ u
check' (NumericLiteral _) Number = return ()
check' (StringLiteral _) String = return ()
check' (BooleanLiteral _) Boolean = return ()
check' (Unary op val) ty = checkUnary op val ty
check' (Binary op left right) ty = checkBinary op left right ty
check' (ArrayLiteral vals) (Array ty) = forM_ vals (\val -> check val ty)
check' (Indexer index vals) ty = check index Number >> check vals (Array ty)
check' (Abs args ret) (Function argTys retTy) = do
  guardWith "Incorrect number of function arguments" (length args == length argTys)
  bindLocalVariables (zip args argTys) $ check ret retTy
check' app@(App _ _) ret = do
  let (f, argss) = unfoldApplication app
  ft <- infer f
  checkFunctionApplications ft argss ret
check' (Var var) ty = do
  ty1 <- lookupVariable var
  repl <- replaceAllTypeSynonyms ty1
  repl `subsumes` ty
check' (TypedValue val ty1) ty2 = do
  kind <- liftCheck $ kindOf ty1
  guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty1 `subsumes` ty2
  check val ty1
check' (Case vals binders) ret = do
  ts <- mapM infer vals
  checkBinders ts ret binders
check' (IfThenElse cond th el) ty = do
  check cond Boolean
  check th ty
  check el ty
check' (ObjectLiteral ps) (Object row) = do
  ensureNoDuplicateProperties ps
  checkProperties ps row False
check' (ObjectUpdate obj ps) (Object row) = do
  ensureNoDuplicateProperties ps
  us <- zip (map fst ps) <$> replicateM (length ps) fresh
  let (propsToCheck, rest) = rowToList row
      propsToRemove = map fst ps
      remainingProps = filter (\(p, _) -> p `notElem` propsToRemove) propsToCheck
  check obj (Object (rowFromList (us ++ remainingProps, rest)))
  checkProperties ps row True
check' (Accessor prop val) ty = do
  rest <- fresh
  check val (Object (RCons prop ty rest))
check' (Block ss) ret = do
  (allCodePathsReturn, _) <- checkBlock M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
check' (Constructor c) ty = do
  env <- getEnv
  modulePath <- checkModulePath <$> get
  case M.lookup (qualify modulePath c) (dataConstructors env) of
    Nothing -> throwError $ "Constructor " ++ show c ++ " is undefined"
    Just ty1 -> do
      repl <- replaceAllTypeSynonyms ty1
      repl `subsumes` ty
check' val (SaturatedTypeSynonym name args) = do
  ty <- expandTypeSynonym name args
  check val ty
check' val ty = throwError $ prettyPrintValue val ++ " does not have type " ++ prettyPrintType ty

checkProperties :: [(String, Value)] -> Row -> Bool -> Subst ()
checkProperties ps row lax = let (ts, r') = rowToList row in go ps ts r' where
  go [] [] REmpty = return ()
  go [] [] u@(RUnknown _) = u ~~ REmpty
  go [] [] (RSkolem _) | lax = return ()
  go [] ((p, _): _) _ | lax = return ()
                      | otherwise = throwError $ prettyPrintValue (ObjectLiteral ps) ++ " does not have property " ++ p
  go ((p,_):_) [] REmpty = throwError $ "Property " ++ p ++ " is not present in closed object type " ++ prettyPrintRow row
  go ((p,v):ps') [] u@(RUnknown _) = do
    ty <- infer v
    rest <- fresh
    u ~~ RCons p ty rest
    go ps' [] rest
  go ((p,v):ps') ts r =
    case lookup p ts of
      Nothing -> do
        ty <- infer v
        rest <- fresh
        r ~~ RCons p ty rest
        go ps' ts rest
      Just ty -> do
        check v ty
        go ps' (delete (p, ty) ts) r
  go _ _ _ = throwError $ prettyPrintValue (ObjectLiteral ps) ++ " does not have type " ++ prettyPrintType (Object row)

unfoldApplication :: Value -> (Value, [[Value]])
unfoldApplication = go []
  where
  go argss (App f args) = go (args:argss) f
  go argss f = (f, argss)

checkFunctionApplications :: Type -> [[Value]] -> Type -> Subst ()
checkFunctionApplications _ [] _ = error "Nullary function application"
checkFunctionApplications fnTy [args] ret = checkFunctionApplication fnTy args ret
checkFunctionApplications fnTy (args:argss) ret = do
  argTys <- mapM (infer) args
  f <- inferFunctionApplication fnTy argTys
  checkFunctionApplications f argss ret

checkFunctionApplication :: Type -> [Value] -> Type -> Subst ()
checkFunctionApplication fnTy args ret = rethrow errorMessage $ checkFunctionApplication' fnTy args ret
  where
  errorMessage msg = "Error applying function of type "
    ++ prettyPrintType fnTy
    ++ " to arguments " ++ intercalate ", " (map prettyPrintValue args)
    ++ ", expecting value of type "
    ++ prettyPrintType ret ++ ":\n" ++ msg

inferFunctionApplication :: Type -> [Type] -> Subst Type
inferFunctionApplication (Function argTys retTy) args = do
  guardWith "Incorrect number of function arguments" (length args == length argTys)
  zipWithM subsumes args argTys
  return retTy
inferFunctionApplication (ForAll ident ty) args = do
  replaced <- replaceVarWithUnknown ident ty
  inferFunctionApplication replaced args
inferFunctionApplication u@(TUnknown _) args = do

  ret <- fresh
  args' <- mapM replaceAllVarsWithUnknowns args
  u ~~ Function args' ret
  return ret
inferFunctionApplication (SaturatedTypeSynonym name tyArgs) args  = do
  ty <- expandTypeSynonym name tyArgs
  inferFunctionApplication ty args
inferFunctionApplication fnTy args = throwError $ "Cannot apply function of type "
  ++ prettyPrintType fnTy
  ++ " to argument(s) of type(s) " ++ intercalate ", " (map prettyPrintType args)

checkFunctionApplication' :: Type -> [Value] -> Type -> Subst ()
checkFunctionApplication' (Function argTys retTy) args ret = do
  guardWith "Incorrect number of function arguments" (length args == length argTys)
  zipWithM (check) args argTys
  retTy `subsumes` ret
checkFunctionApplication' (ForAll ident ty) args ret = do
  replaced <- replaceVarWithUnknown ident ty
  checkFunctionApplication replaced args ret
checkFunctionApplication' u@(TUnknown _) args ret = do
  tyArgs <- mapM (\arg -> infer arg >>= replaceAllVarsWithUnknowns) args
  u ~~ Function tyArgs ret
checkFunctionApplication' (SaturatedTypeSynonym name tyArgs) args ret = do
  ty <- expandTypeSynonym name tyArgs
  checkFunctionApplication' ty args ret
checkFunctionApplication' fnTy args ret = throwError $ "Applying a function of type "
  ++ prettyPrintType fnTy
  ++ " to argument(s) " ++ intercalate ", " (map prettyPrintValue args)
  ++ " does not yield a value of type " ++ prettyPrintType ret ++ "."

subsumes :: Type -> Type -> Subst ()
subsumes (ForAll ident ty1) ty2 = do
  replaced <- replaceVarWithUnknown ident ty1
  replaced `subsumes` ty2
subsumes (Function args1 ret1) (Function args2 ret2) = do
  zipWithM subsumes args2 args1
  ret1 `subsumes` ret2
subsumes ty1 ty2 = ty1 ~~ ty2

