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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module Language.PureScript.TypeChecker.Types (
    typesOf
) where

import Data.List
import Data.Maybe (isNothing, isJust, fromMaybe)
import qualified Data.Data as D
import Data.Generics (everything, mkT, something, everywhere, mkQ)

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
import Control.Monad.Reader

import Control.Applicative
import Control.Arrow (Arrow(..))

import qualified Data.Map as M
import Data.Function (on)

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
  apply s (RCons name ty r) = RCons name (apply s ty) (apply s r)
  apply _ t = t
  unknowns (TUnknown (Unknown u)) = [u]
  unknowns (SaturatedTypeSynonym _ tys) = concatMap unknowns tys
  unknowns (ForAll _ ty) = unknowns ty
  unknowns (Array t) = unknowns t
  unknowns (Object r) = unknowns r
  unknowns (Function args ret) = concatMap unknowns args ++ unknowns ret
  unknowns (TypeApp t1 t2) = unknowns t1 ++ unknowns t2
  unknowns (RCons _ ty r) = unknowns ty ++ unknowns r
  unknowns _ = []

unifyTypes :: Type -> Type -> Subst ()
unifyTypes t1 t2 = rethrow (\e -> "Error unifying type " ++ prettyPrintType t1 ++ " with type " ++ prettyPrintType t2 ++ ":\n" ++ e) $ do
  unifyTypes' t1 t2
  where
  unifyTypes' (TUnknown u1) (TUnknown u2) | u1 == u2 = return ()
  unifyTypes' (TUnknown u) t = replace u t
  unifyTypes' t (TUnknown u) = replace u t
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
    env <- getEnv
    moduleName <- substCurrentModule `fmap` ask
    guardWith ("Cannot unify " ++ show c1 ++ " with " ++ show c2 ++ ".") (typeConstructorsAreEqual env moduleName c1 c2)
  unifyTypes' (TypeApp t3 t4) (TypeApp t5 t6) = do
    t3 `unifyTypes` t5
    t4 `unifyTypes` t6
  unifyTypes' (Skolem s1) (Skolem s2) | s1 == s2 = return ()
  unifyTypes' r1@(RCons _ _ _) r2 = unifyRows r1 r2
  unifyTypes' r1 r2@(RCons _ _ _) = unifyRows r1 r2
  unifyTypes' r1@REmpty r2 = unifyRows r1 r2
  unifyTypes' r1 r2@REmpty = unifyRows r1 r2
  unifyTypes' t3 t4 = throwError $ "Cannot unify " ++ prettyPrintType t3 ++ " with " ++ prettyPrintType t4 ++ "."

unifyRows :: Type -> Type -> Subst ()
unifyRows r1 r2 =
  let
    (s1, r1') = rowToList r1
    (s2, r2') = rowToList r2
    int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
    sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
    sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
  in do
    forM_ int (uncurry (~~))
    unifyRows' sd1 r1' sd2 r2'
  where
  unifyRows' :: [(String, Type)] -> Type -> [(String, Type)] -> Type -> Subst ()
  unifyRows' [] (TUnknown u) sd r = replace u (rowFromList (sd, r))
  unifyRows' sd r [] (TUnknown u) = replace u (rowFromList (sd, r))
  unifyRows' ((name, ty):row) r others u@(TUnknown un) = do
    occursCheck un ty
    forM_ row $ \(_, t) -> occursCheck un t
    u' <- fresh
    u ~~ RCons name ty u'
    unifyRows' row r others u'
  unifyRows' [] REmpty [] REmpty = return ()
  unifyRows' [] (TypeVar v1) [] (TypeVar v2) | v1 == v2 = return ()
  unifyRows' [] (Skolem s1) [] (Skolem s2) | s1 == s2 = return ()
  unifyRows' sd3 r3 sd4 r4 = throwError $ "Cannot unify " ++ prettyPrintRow (rowFromList (sd3, r3)) ++ " with " ++ prettyPrintRow (rowFromList (sd4, r4)) ++ "."

typeConstructorsAreEqual :: Environment -> ModuleName -> Qualified ProperName -> Qualified ProperName -> Bool
typeConstructorsAreEqual env moduleName = (==) `on` canonicalizeType moduleName env

typesOf :: ModuleName -> [(Ident, Value)] -> Check [(Ident, (Value, Type))]
typesOf moduleName vals = do
  tys <- fmap (\(tys, s) -> map (\(ident, (val, ty)) -> (ident, (overTypes (apply s) val, apply s ty))) tys)
         . runSubst (SubstContext moduleName) $ do
    let es = map isTyped vals
        typed = filter (isJust . snd . snd) es
        untyped = filter (isNothing . snd . snd) es
        typedDict = map (\(ident, (_, Just ty)) -> (ident, ty)) typed
    untypedNames <- replicateM (length untyped) fresh
    let untypedDict = zip (map fst untyped) untypedNames
        dict = M.fromList (map (\(ident, ty) -> ((moduleName, ident), (ty, LocalVariable))) $ typedDict ++ untypedDict)
    forM es $ \e -> do
      triple@(_, (val, ty)) <- case e of
        (ident, (val, Just ty)) -> do
          kind <- liftCheck $ kindOf moduleName ty
          guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
          ty' <- replaceAllTypeSynonyms ty
          val' <- bindNames dict $ check val ty'
          return (ident, (val', ty'))
        (ident, (val, Nothing)) -> do
          TypedValue val' ty <- bindNames dict $ infer val
          ty ~~ fromMaybe (error "name not found in dictionary") (lookup ident untypedDict)
          return (ident, (val', ty))
      when (moduleName == ModuleName (ProperName "Main") && fst e == Ident "main") $ do
        [eff, a] <- replicateM 2 fresh
        ty ~~ TypeApp (TypeApp (TypeConstructor (Qualified (Just (ModuleName (ProperName "Eff"))) (ProperName "Eff"))) eff) a
      escapeCheck val ty
      return triple
  forM_ tys $ skolemEscapeCheck . snd . snd
  return $ map (\(ident, (val, ty)) -> ( ident, ( overTypes (desaturateAllTypeSynonyms . setifyAll) val
                                                , varIfUnknown . desaturateAllTypeSynonyms . setifyAll $ ty))) tys

isTyped :: (Ident, Value) -> (Ident, (Value, Maybe Type))
isTyped (name, TypedValue value ty) = (name, (value, Just ty))
isTyped (name, value) = (name, (value, Nothing))

overTypes :: (Type -> Type) -> Value -> Value
overTypes f = everywhere (mkT f)

escapeCheck :: Value -> Type -> Subst ()
escapeCheck value ty = do
  subst <- substSubst <$> getSubstState
  let visibleUnknowns = nub $ unknowns $ apply subst ty
  let allUnknowns = findAllTypes value
  forM_ allUnknowns $ \t -> do
    let unsolvedUnknowns = nub . unknowns $ apply subst t
    guardWith "Escape check fails" $ null $ unsolvedUnknowns \\ visibleUnknowns

findAllTypes :: Value -> [Type]
findAllTypes = everything (++) (mkQ [] go)
  where
  go (TypedValue _ ty) = [ty]
  go _ = []

skolemEscapeCheck :: Type -> Check ()
skolemEscapeCheck ty =
  case something (mkQ Nothing findSkolems) ty of
    Nothing -> return ()
    Just _ -> throwError $ "Skolem variables cannot escape. Consider adding a type signature." ++ show ty
  where
    findSkolems (Skolem _) = return ()
    findSkolems _ = mzero

setify :: Type -> Type
setify = rowFromList . first (M.toList . M.fromList) . rowToList

setifyAll :: (D.Data d) => d -> d
setifyAll = everywhere (mkT setify)

varIfUnknown :: Type -> Type
varIfUnknown ty =
  let unks = nub $ unknowns ty
      toName = (:) 't' . show
      ty' = everywhere (mkT typeToVar) $ ty
      typeToVar :: Type -> Type
      typeToVar (TUnknown (Unknown u)) = TypeVar (toName u)
      typeToVar t = t
  in mkForAll (sort . map toName $ unks) ty'

replaceAllTypeVars :: (D.Data d) => [(String, Type)] -> d -> d
replaceAllTypeVars = foldl' (\f (name, ty) -> replaceTypeVars name ty . f) id

replaceAllVarsWithUnknowns :: Type -> Subst Type
replaceAllVarsWithUnknowns (ForAll ident ty) = replaceVarWithUnknown ident ty >>= replaceAllVarsWithUnknowns
replaceAllVarsWithUnknowns ty = return ty

replaceVarWithUnknown :: String -> Type -> Subst Type
replaceVarWithUnknown ident ty = do
  tu <- fresh
  return $ replaceTypeVars ident tu $ ty

replaceAllTypeSynonyms :: (Functor m, MonadState CheckState m, MonadReader SubstContext m, MonadError String m) => (D.Data d) => d -> m d
replaceAllTypeSynonyms d = do
  env <- getEnv
  moduleName <- substCurrentModule <$> ask
  let syns = map (\((path, name), (args, _)) -> ((path, name), length args)) . M.toList $ typeSynonyms env
  either throwError return $ saturateAllTypeSynonyms env moduleName syns d

desaturateAllTypeSynonyms :: (D.Data d) => d -> d
desaturateAllTypeSynonyms = everywhere (mkT replaceSaturatedTypeSynonym)
  where
  replaceSaturatedTypeSynonym (SaturatedTypeSynonym name args) = foldl TypeApp (TypeConstructor name) args
  replaceSaturatedTypeSynonym t = t

expandTypeSynonym :: Qualified ProperName -> [Type] -> Subst Type
expandTypeSynonym name args = do
  env <- getEnv
  moduleName <- substCurrentModule `fmap` ask
  case M.lookup (canonicalizeType moduleName env name) (typeSynonyms env) of
    Just (synArgs, body) -> return $ replaceAllTypeVars (zip synArgs args) body
    Nothing -> error "Type synonym was not defined"

ensureNoDuplicateProperties :: (MonadError String m) => [(String, Value)] -> m ()
ensureNoDuplicateProperties ps = guardWith "Duplicate property names" $ length (nub . map fst $ ps) == length ps

infer :: Value -> Subst Value
infer val = rethrow (\e -> "Error inferring type of term " ++ prettyPrintValue val ++ ":\n" ++ e) $ infer' val

infer' :: Value -> Subst Value
infer' v@(NumericLiteral _) = return $ TypedValue v Number
infer' v@(StringLiteral _) = return $ TypedValue v String
infer' v@(BooleanLiteral _) = return $ TypedValue v Boolean
infer' (ArrayLiteral vals) = do
  ts <- mapM infer vals
  els <- fresh
  forM_ ts $ \(TypedValue _ t) -> els ~~ Array t
  return $ TypedValue (ArrayLiteral ts) els
infer' (Unary op val) = do
  v <- infer val
  inferUnary op v
infer' (Binary op left right) = do
  v1 <- infer left
  v2 <- infer right
  inferBinary op v1 v2
infer' (ObjectLiteral ps) = do
  ensureNoDuplicateProperties ps
  ts <- mapM (infer . snd) ps
  let fields = zipWith (\name (TypedValue _ t) -> (name, t)) (map fst ps) ts
      ty = Object $ rowFromList (fields, REmpty)
  return $ TypedValue (ObjectLiteral (zip (map fst ps) ts)) ty
infer' (ObjectUpdate o ps) = do
  ensureNoDuplicateProperties ps
  row <- fresh
  newVals <- zipWith (\(name, _) t -> (name, t)) ps <$> mapM (infer . snd) ps
  let newTys = map (\(name, TypedValue _ ty) -> (name, ty)) newVals
  oldTys <- zip (map fst ps) <$> replicateM (length ps) fresh
  o' <- check o $ Object $ rowFromList (oldTys, row)
  return $ TypedValue (ObjectUpdate o' newVals) $ Object $ rowFromList (newTys, row)
infer' (Indexer index val) = do
  el <- fresh
  index' <- check index Number
  val' <- check val (Array el)
  return $ TypedValue (Indexer (TypedValue index' Number) (TypedValue val' (Array el))) el
infer' (Accessor prop val) = do
  typed@(TypedValue _ objTy) <- infer val
  propTy <- inferProperty objTy prop
  case propTy of
    Nothing -> do
      field <- fresh
      rest <- fresh
      objTy `subsumes` Object (RCons prop field rest)
      return $ TypedValue (Accessor prop typed) field
    Just ty -> return $ TypedValue (Accessor prop typed) ty
infer' (Abs args ret) = do
  ts <- replicateM (length args) fresh
  moduleName <- substCurrentModule <$> ask
  bindLocalVariables moduleName (zip args ts) $ do
    body@(TypedValue _ bodyTy) <- infer' ret
    return $ TypedValue (Abs args body) $ Function ts bodyTy
infer' (App f args) = do
  f'@(TypedValue _ ft) <- infer f
  ret <- fresh
  app <- checkFunctionApplication f' ft args ret
  return $ TypedValue app ret
infer' (Var var) = do
  moduleName <- substCurrentModule <$> ask
  ty <- lookupVariable moduleName var
  ty' <- replaceAllTypeSynonyms ty
  case ty' of
    ConstrainedType constraints _ -> return $ TypedValue (App (Var var) [TypeClassDictionary constraints]) ty'
    _ -> return $ TypedValue (Var var) ty'
infer' (Block ss) = do
  ret <- fresh
  (allCodePathsReturn, _, ss') <- checkBlock M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
  return $ TypedValue (Block ss') ret
infer' v@(Constructor c) = do
  env <- getEnv
  moduleName <- substCurrentModule `fmap` ask
  case M.lookup (qualify moduleName c) (dataConstructors env) of
    Nothing -> throwError $ "Constructor " ++ show c ++ " is undefined"
    Just (ty, _) -> do ty' <- replaceAllTypeSynonyms ty
                       return $ TypedValue v ty'
infer' (Case vals binders) = do
  ts <- mapM infer vals
  ret <- fresh
  binders' <- checkBinders (map (\(TypedValue _ t) -> t) ts) ret binders
  return $ TypedValue (Case ts binders') ret
infer' (IfThenElse cond th el) = do
  cond' <- check cond Boolean
  v2@(TypedValue _ t2) <- infer th
  v3@(TypedValue _ t3) <- infer el
  t2 ~~ t3
  return $ TypedValue (IfThenElse cond' v2 v3) t2
infer' (TypedValue val ty) = do
  moduleName <- substCurrentModule <$> ask
  kind <- liftCheck $ kindOf moduleName ty
  guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty' <- replaceAllTypeSynonyms ty
  val' <- check val ty'
  return $ TypedValue val' ty
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

inferUnary :: UnaryOperator -> Value -> Subst Value
inferUnary op (TypedValue val valTy) =
  case fromMaybe (error "Invalid operator") $ lookup op unaryOps of
    (valTy', resTy) -> do
      valTy' ~~ valTy
      return $ TypedValue (Unary op val) resTy
inferUnary _ _ = error "Invalid arguments to inferUnary"

checkUnary :: UnaryOperator -> Value -> Type -> Subst Value
checkUnary op val res =
  case fromMaybe (error "Invalid operator") $ lookup op unaryOps of
    (valTy, resTy) -> do
      res ~~ resTy
      val' <- check val valTy
      return $ Unary op val'

unaryOps :: [(UnaryOperator, (Type, Type))]
unaryOps = [ (Negate, (Number, Number))
           , (Not, (Boolean, Boolean))
           , (BitwiseNot, (Number, Number))
           ]

inferBinary :: BinaryOperator -> Value -> Value -> Subst Value
inferBinary op left@(TypedValue _ leftTy) right@(TypedValue _ rightTy) | isEqualityTest op = do
  leftTy ~~ rightTy
  return $ TypedValue (Binary op left right) Boolean
inferBinary op left@(TypedValue _ leftTy) right@(TypedValue _ rightTy) =
  case fromMaybe (error "Invalid operator") $ lookup op binaryOps of
    (valTy, resTy) -> do
      leftTy ~~ valTy
      rightTy ~~ valTy
      return $ TypedValue (Binary op left right) resTy
inferBinary _ _ _ = error "Invalid arguments to inferBinary"

checkBinary :: BinaryOperator -> Value -> Value -> Type -> Subst Value
checkBinary op left right res | isEqualityTest op = do
  res ~~ Boolean
  left'@(TypedValue _ t1) <- infer left
  right'@(TypedValue _ t2) <- infer right
  t1 ~~ t2
  return $ Binary op left' right'
checkBinary op left right res =
  case fromMaybe (error "Invalid operator") $ lookup op binaryOps of
    (valTy, resTy) -> do
      res ~~ resTy
      left' <- check left valTy
      right' <- check right valTy
      return $ Binary op left' right'

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
  moduleName <- substCurrentModule <$> ask
  case M.lookup (qualify moduleName ctor) (dataConstructors env) of
    Just (ty, _) -> do
      ty `subsumes` val
      return M.empty
    _ -> throwError $ "Constructor " ++ show ctor ++ " is not defined"
inferBinder val (UnaryBinder ctor binder) = do
  env <- getEnv
  moduleName <- substCurrentModule <$> ask
  case M.lookup (qualify moduleName ctor) (dataConstructors env) of
    Just (ty, _) -> do
      fn <- replaceAllVarsWithUnknowns ty
      case fn of
        Function [obj] ret -> do
          val `subsumes` ret
          inferBinder obj binder
        _ -> throwError $ "Constructor " ++ show ctor ++ " is not a unary constructor"
    _ -> throwError $ "Constructor " ++ show ctor ++ " is not defined"
inferBinder val (ObjectBinder props) = do
  row <- fresh
  rest <- fresh
  m1 <- inferRowProperties row rest props
  val ~~ Object row
  return m1
  where
  inferRowProperties :: Type -> Type -> [(String, Binder)] -> Subst (M.Map Ident Type)
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

checkBinders :: [Type] -> Type -> [([Binder], Maybe Guard, Value)] -> Subst [([Binder], Maybe Guard, Value)]
checkBinders _ _ [] = return []
checkBinders nvals ret ((binders, grd, val):bs) = do
  moduleName <- substCurrentModule <$> ask
  m1 <- M.unions <$> zipWithM inferBinder nvals binders
  r <- bindLocalVariables moduleName (M.toList m1) $ do
    val' <- check val ret
    case grd of
      Nothing -> return (binders, Nothing, val')
      Just g -> do
        g' <- check g Boolean
        return (binders, Just g', val')
  rs <- checkBinders nvals ret bs
  return $ r : rs

assignVariable :: Ident -> Subst ()
assignVariable name = do
  env <- checkEnv <$> get
  moduleName <- substCurrentModule <$> ask
  case M.lookup (moduleName, name) (names env) of
    Just (_, LocalVariable) -> throwError $ "Variable with name " ++ show name ++ " already exists."
    _ -> return ()

checkStatement :: M.Map Ident Type -> Type -> Statement -> Subst (Bool, M.Map Ident Type, Statement)
checkStatement mass _ (VariableIntroduction name val) = do
  assignVariable name
  val'@(TypedValue _ t) <- infer val
  return (False, M.insert name t mass, VariableIntroduction name val')
checkStatement mass _ (Assignment ident val) = do
  val'@(TypedValue _ t) <- infer val
  case M.lookup ident mass of
    Nothing -> throwError $ "No local variable with name " ++ show ident
    Just ty -> do t ~~ ty
                  return (False, mass, Assignment ident val')
checkStatement mass ret (While val inner) = do
  val' <- check val Boolean
  (allCodePathsReturn, _, inner') <- checkBlock mass ret inner
  return (allCodePathsReturn, mass, While val' inner')
checkStatement mass ret (If ifst) = do
  (allCodePathsReturn, ifst') <- checkIfStatement mass ret ifst
  return (allCodePathsReturn, mass, If ifst')
checkStatement mass ret (For ident start end inner) = do
  moduleName <- substCurrentModule <$> ask
  assignVariable ident
  start' <- check start Number
  end' <- check end Number
  (allCodePathsReturn, _, inner') <- bindLocalVariables moduleName [(ident, Number)] $ checkBlock mass ret inner
  return (allCodePathsReturn, mass, For ident start' end' inner')
checkStatement mass ret (Return val) = do
  val' <- check val ret
  return (True, mass, Return val')

checkIfStatement :: M.Map Ident Type -> Type -> IfStatement -> Subst (Bool, IfStatement)
checkIfStatement mass ret (IfStatement val thens Nothing) = do
  val' <- check val Boolean
  (_, _, thens') <- checkBlock mass ret thens
  return (False, IfStatement val' thens' Nothing)
checkIfStatement mass ret (IfStatement val thens (Just elses)) = do
  val' <- check val Boolean
  (allCodePathsReturn1, _, thens') <- checkBlock mass ret thens
  (allCodePathsReturn2, elses') <- checkElseStatement mass ret elses
  return (allCodePathsReturn1 && allCodePathsReturn2, IfStatement val' thens' (Just elses'))

checkElseStatement :: M.Map Ident Type -> Type -> ElseStatement -> Subst (Bool, ElseStatement)
checkElseStatement mass ret (Else elses) = do
  (allCodePathsReturn, _, elses') <- checkBlock mass ret elses
  return (allCodePathsReturn, Else elses')
checkElseStatement mass ret (ElseIf ifst) = (id *** ElseIf) <$> checkIfStatement mass ret ifst

checkBlock :: M.Map Ident Type -> Type -> [Statement] -> Subst (Bool, M.Map Ident Type, [Statement])
checkBlock mass _ [] = return (False, mass, [])
checkBlock mass ret (s:ss) = do
  moduleName <- substCurrentModule <$> ask
  (b1, mass1, s') <- checkStatement mass ret s
  bindLocalVariables moduleName (M.toList mass1) $ case (b1, ss) of
    (True, []) -> return (True, mass1, [s'])
    (True, _) -> throwError "Unreachable code"
    (False, ss') -> do
      (b, m, ss'') <- checkBlock mass1 ret ss'
      return (b, m, s':ss'')

skolemize :: String -> Type -> Subst Type
skolemize ident ty = do
  tsk <- Skolem <$> fresh'
  return $ replaceTypeVars ident tsk ty

check :: Value -> Type -> Subst Value
check val ty = rethrow errorMessage $ check' val ty
  where
  errorMessage msg =
    "Error checking type of term " ++
    prettyPrintValue val ++
    " against type " ++
    prettyPrintType ty ++
    ":\n" ++
    msg

check' :: Value -> Type -> Subst Value
check' val (ForAll idents ty) = do
  sk <- skolemize idents ty
  check val sk
check' val (ConstrainedType constraints ty) = do
  val' <- check val ty
  return $ Abs (map (\_ -> Ident "__dict") constraints) val'
check' val u@(TUnknown _) = do
  val'@(TypedValue _ ty) <- infer val
  -- Don't unify an unknown with an inferred polytype
  ty' <- replaceAllVarsWithUnknowns ty
  ty' ~~ u
  return val'
check' v@(NumericLiteral _) Number = return v
check' v@(StringLiteral _) String = return v
check' v@(BooleanLiteral _) Boolean = return v
check' (Unary op val) ty = checkUnary op val ty
check' (Binary op left right) ty = checkBinary op left right ty
check' (ArrayLiteral vals) (Array ty) = ArrayLiteral <$> forM vals (\val -> check val ty)
check' (Indexer index vals) ty = do
  index' <- check index Number
  vals' <- check vals (Array ty)
  return $ Indexer index' vals'
check' (Abs args ret) (Function argTys retTy) = do
  moduleName <- substCurrentModule <$> ask
  guardWith "Incorrect number of function arguments" (length args == length argTys)
  ret' <- bindLocalVariables moduleName (zip args argTys) $ check ret retTy
  return $ Abs args ret'
check' (App f args) ret = do
  f'@(TypedValue _ ft) <- infer f
  app <- checkFunctionApplication f' ft args ret
  return $ app
check' (Var var) ty = do
  moduleName <- substCurrentModule <$> ask
  ty1 <- lookupVariable moduleName var
  repl <- replaceAllTypeSynonyms ty1
  repl `subsumes` ty
  return $ Var var
check' (TypedValue val ty1) ty2 = do
  moduleName <- substCurrentModule <$> ask
  kind <- liftCheck $ kindOf moduleName ty1
  guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  ty1 `subsumes` ty2
  val' <- check val ty1
  return $ TypedValue val' ty1
check' (Case vals binders) ret = do
  vals' <- mapM infer vals
  let ts = map (\(TypedValue _ t) -> t) vals'
  binders' <- checkBinders ts ret binders
  return $ Case vals' binders'
check' (IfThenElse cond th el) ty = do
  cond' <- check cond Boolean
  th' <- check th ty
  el' <- check el ty
  return $ IfThenElse cond' th' el'
check' (ObjectLiteral ps) (Object row) = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties ps row False
  return $ ObjectLiteral ps'
check' (ObjectUpdate obj ps) (Object row) = do
  ensureNoDuplicateProperties ps
  us <- zip (map fst ps) <$> replicateM (length ps) fresh
  let (propsToCheck, rest) = rowToList row
      propsToRemove = map fst ps
      remainingProps = filter (\(p, _) -> p `notElem` propsToRemove) propsToCheck
  obj' <- check obj (Object (rowFromList (us ++ remainingProps, rest)))
  ps' <- checkProperties ps row True
  return $ ObjectUpdate obj' ps'
check' (Accessor prop val) ty = do
  rest <- fresh
  val' <- check val (Object (RCons prop ty rest))
  return $ Accessor prop val'
check' (Block ss) ret = do
  (allCodePathsReturn, _, ss') <- checkBlock M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
  return $ Block ss'
check' (Constructor c) ty = do
  env <- getEnv
  moduleName <- substCurrentModule <$> ask
  case M.lookup (qualify moduleName c) (dataConstructors env) of
    Nothing -> throwError $ "Constructor " ++ show c ++ " is undefined"
    Just (ty1, _) -> do
      repl <- replaceAllTypeSynonyms ty1
      repl `subsumes` ty
      return $ Constructor c
check' val (SaturatedTypeSynonym name args) = do
  ty <- expandTypeSynonym name args
  check val ty
check' val ty = throwError $ prettyPrintValue val ++ " does not have type " ++ prettyPrintType ty

checkProperties :: [(String, Value)] -> Type -> Bool -> Subst [(String, Value)]
checkProperties ps row lax = let (ts, r') = rowToList row in go ps ts r' where
  go [] [] REmpty = return []
  go [] [] u@(TUnknown _) = do u ~~ REmpty
                               return []
  go [] [] (Skolem _) | lax = return []
  go [] ((p, _): _) _ | lax = return []
                      | otherwise = throwError $ prettyPrintValue (ObjectLiteral ps) ++ " does not have property " ++ p
  go ((p,_):_) [] REmpty = throwError $ "Property " ++ p ++ " is not present in closed object type " ++ prettyPrintRow row
  go ((p,v):ps') [] u@(TUnknown _) = do
    v'@(TypedValue _ ty) <- infer v
    rest <- fresh
    u ~~ RCons p ty rest
    ps'' <- go ps' [] rest
    return $ (p, v') : ps''
  go ((p,v):ps') ts r =
    case lookup p ts of
      Nothing -> do
        v'@(TypedValue _ ty) <- infer v
        rest <- fresh
        r ~~ RCons p ty rest
        ps'' <- go ps' ts rest
        return $ (p, v') : ps''
      Just ty -> do
        v' <- check v ty
        ps'' <- go ps' (delete (p, ty) ts) r
        return $ (p, v') : ps''
  go _ _ _ = throwError $ prettyPrintValue (ObjectLiteral ps) ++ " does not have type " ++ prettyPrintType (Object row)

checkFunctionApplication :: Value -> Type -> [Value] -> Type -> Subst Value
checkFunctionApplication fn fnTy args ret = rethrow errorMessage $ checkFunctionApplication' fn fnTy args ret
  where
  errorMessage msg = "Error applying function of type "
    ++ prettyPrintType fnTy
    ++ " to arguments " ++ intercalate ", " (map prettyPrintValue args)
    ++ ":\n" ++ msg

checkFunctionApplication' :: Value -> Type -> [Value] -> Type -> Subst Value
checkFunctionApplication' fn (Function argTys retTy) args ret = do
  guardWith "Incorrect number of function arguments" (length args == length argTys)
  args' <- zipWithM check args argTys
  retTy `subsumes` ret
  return $ App fn args'
checkFunctionApplication' fn (ForAll ident ty) args ret = do
  replaced <- replaceVarWithUnknown ident ty
  checkFunctionApplication fn replaced args ret
checkFunctionApplication' fn u@(TUnknown _) args ret = do
  args' <- mapM (\arg -> infer arg >>= \(TypedValue v t) -> TypedValue v <$> replaceAllVarsWithUnknowns t) args
  let tys = map (\(TypedValue _ t) -> t) args'
  u ~~ Function tys ret
  return $ App fn args'
checkFunctionApplication' fn (SaturatedTypeSynonym name tyArgs) args ret = do
  ty <- expandTypeSynonym name tyArgs
  checkFunctionApplication fn ty args ret
checkFunctionApplication' fn (ConstrainedType constraints fnTy) args ret = do
  checkFunctionApplication' (App fn [TypeClassDictionary constraints]) fnTy args ret
checkFunctionApplication' _ fnTy args ret = throwError $ "Applying a function of type "
  ++ prettyPrintType fnTy
  ++ " to argument(s) " ++ intercalate ", " (map prettyPrintValue args)
  ++ " does not yield a value of type " ++ prettyPrintType ret ++ "."

subsumes :: Type -> Type -> Subst ()
subsumes (ForAll ident ty1) ty2 = do
  replaced <- replaceVarWithUnknown ident ty1
  replaced `subsumes` ty2
subsumes (Function args1 ret1) (Function args2 ret2) = do
  zipWithM_ subsumes args2 args1
  ret1 `subsumes` ret2
subsumes ty1 ty2 = ty1 ~~ ty2

