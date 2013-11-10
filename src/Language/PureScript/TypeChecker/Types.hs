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
    TypeConstraint(..),
    TypeSolution(..),
    typeOf
) where

import Debug.Trace

import Data.List
import Data.Function
import qualified Data.Data as D
import Data.Generics (everywhere, everywhereM, everything, mkT, mkM, mkQ, extM, extQ)

import Language.PureScript.Values
import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Kinds
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.Pretty

import Control.Monad.State
import Control.Monad.Error

import Control.Applicative
import Control.Arrow (Kleisli(..), (***), (&&&), first)
import qualified Control.Category as C

import qualified Data.Map as M

data TypeConstraintOrigin
  = ValueOrigin Value
  | BinderOrigin Binder
  | AssignmentTargetOrigin Ident deriving (Show, D.Data, D.Typeable)

prettyPrintOrigin :: TypeConstraintOrigin -> String
prettyPrintOrigin (ValueOrigin val) = prettyPrintValue val
prettyPrintOrigin (BinderOrigin binder) = prettyPrintBinder binder
prettyPrintOrigin (AssignmentTargetOrigin ident) = show ident

data TypeConstraint
  = TypeConstraint Int Type TypeConstraintOrigin
  | RowConstraint Int Row TypeConstraintOrigin deriving (Show, D.Data, D.Typeable)

newtype TypeSolution = TypeSolution { runTypeSolution :: (Int -> Type, Int -> Row) }

emptyTypeSolution :: TypeSolution
emptyTypeSolution = TypeSolution (TUnknown, RUnknown)

isFunction :: Value -> Bool
isFunction (Abs _ _) = True
isFunction (TypedValue untyped _) = isFunction untyped
isFunction _ = False

allConstraints :: Ident -> Value -> Check ([TypeConstraint], Int)
allConstraints name val | isFunction val = do
  me <- fresh
  (cs, n) <- typeConstraints (M.singleton name me) val
  return (TypeConstraint me (TUnknown n) (ValueOrigin val): cs, n)
allConstraints _ val = typeConstraints M.empty val

typeOf :: Ident -> Value -> Check PolyType
typeOf name val = do
  (cs, n) <- allConstraints name val
  desugared <- replaceAllTypeSynonyms cs
  solution <- solveTypeConstraints desugared emptyTypeSolution
  let ty = fst (runTypeSolution solution) n
  allUnknownsBecameQuantified desugared solution ty
  return $ varIfUnknown $ desaturateAllTypeSynonyms $ setifyAll ty

allUnknownsBecameQuantified :: [TypeConstraint] -> TypeSolution -> Type -> Check ()
allUnknownsBecameQuantified cs solution ty = do
  let
    typesMentioned = findUnknownTypes ty
    unknownTypes = nub $ flip concatMap cs $ \c -> case c of
      TypeConstraint u t _ -> u : findUnknownTypes t
      RowConstraint _ r _ -> findUnknownTypes r
    unsolvedTypes = filter (\n -> TUnknown n == fst (runTypeSolution solution) n) unknownTypes
  guardWith "Unsolved type variable" $ null $ unsolvedTypes \\ typesMentioned
  let
    rowsMentioned = findUnknownRows ty
    unknownRows = nub $ flip concatMap cs $ \c -> case c of
      TypeConstraint _ t _ -> findUnknownRows t
      RowConstraint u r _ -> u : findUnknownRows r
    unsolvedRows = filter (\n -> RUnknown n == snd (runTypeSolution solution) n) unknownRows
  guardWith "Unsolved row variable" $ null $ unsolvedRows \\ rowsMentioned

setify :: Row -> Row
setify = rowFromList . first (M.toList . M.fromList) . rowToList

setifyAll :: (D.Data d) => d -> d
setifyAll = everywhere (mkT setify)

findUnknownTypes :: (D.Data d) => d -> [Int]
findUnknownTypes = everything (++) (mkQ [] f)
  where
  f :: Type -> [Int]
  f (TUnknown n) = [n]
  f _ = []

findTypeVars :: (D.Data d) => d -> [String]
findTypeVars = everything (++) (mkQ [] f)
  where
  f :: Type -> [String]
  f (TypeVar v) = [v]
  f _ = []

findUnknownRows :: (D.Data d) => d -> [Int]
findUnknownRows = everything (++) (mkQ [] f)
  where
  f :: Row -> [Int]
  f (RUnknown n) = [n]
  f _ = []

varIfUnknown :: Type -> PolyType
varIfUnknown ty =
  let
    (ty', m) = flip runState M.empty $ everywhereM (flip extM g $ mkM f) ty
  in
    PolyType (sort $ nub $ M.elems m ++ findTypeVars ty) ty'
  where
  f :: Type -> State (M.Map Int String) Type
  f (TUnknown n) = do
    m <- get
    case M.lookup n m of
      Nothing -> do
        let name = 't' : show (M.size m)
        put $ M.insert n name m
        return $ TypeVar name
      Just name -> return $ TypeVar name
  f t = return t
  g :: Row -> State (M.Map Int String) Row
  g (RUnknown n) = do
    m <- get
    case M.lookup n m of
      Nothing -> do
        let name = 'r' : show (M.size m)
        put $ M.insert n name m
        return $ RowVar name
      Just name -> return $ RowVar name
  g r = return r

replaceTypeVars :: M.Map String Type -> Type -> Type
replaceTypeVars m = everywhere (mkT replace)
  where
  replace (TypeVar v) = case M.lookup v m of
    Just ty -> ty
    _ -> TypeVar v
  replace t = t

replaceVarsWithUnknowns :: [String] -> Type -> Check Type
replaceVarsWithUnknowns idents = flip evalStateT M.empty . everywhereM (flip extM f $ mkM g)
  where
  f :: Type -> StateT (M.Map String Int) Check Type
  f (TypeVar var) | var `elem` idents = do
    m <- get
    n <- lift fresh
    case M.lookup var m of
      Nothing -> do
        put (M.insert var n m)
        return $ TUnknown n
      Just u -> return $ TUnknown u
  f t = return t
  g :: Row -> StateT (M.Map String Int) Check Row
  g (RowVar var) | var `elem` idents = do
    m <- get
    n <- lift fresh
    case M.lookup var m of
      Nothing -> do
        put (M.insert var n m)
        return $ RUnknown n
      Just u -> return $ RUnknown u
  g r = return r

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

replaceType :: (D.Data d) => Int -> Type -> d -> d
replaceType n t = everywhere (mkT go)
  where
  go (TUnknown m) | m == n = t
  go t = t

replaceRow :: (D.Data d) => Int -> Row -> d -> d
replaceRow n r = everywhere (mkT go)
  where
  go (RUnknown m) | m == n = r
  go r = r

typeOccursCheck :: Int -> Type -> Check ()
typeOccursCheck u (TUnknown _) = return ()
typeOccursCheck u t = when (occursCheck u t) $ throwError $ "Occurs check failed: " ++ show u ++ " = " ++ prettyPrintType t

rowOccursCheck :: Int -> Row -> Check ()
rowOccursCheck u (RUnknown _) = return ()
rowOccursCheck u r = when (occursCheck u r) $ throwError $ "Occurs check failed: " ++ show u ++ " = " ++ prettyPrintRow r

occursCheck :: (D.Data d) => Int -> d -> Bool
occursCheck u = everything (||) $ flip extQ g $ mkQ False f
  where
  f (TUnknown u') | u' == u = True
  f _ = False
  g (RUnknown u') | u' == u = True
  g _ = False

typesToRow :: [(String, Type)] -> Row
typesToRow [] = REmpty
typesToRow ((name, ty):tys) = RCons name ty (typesToRow tys)

rowToList :: Row -> ([(String, Type)], Row)
rowToList (RCons name ty row) = let (tys, rest) = rowToList row
                               in ((name, ty):tys, rest)
rowToList r = ([], r)

rowFromList :: ([(String, Type)], Row) -> Row
rowFromList ([], r) = r
rowFromList ((name, t):ts, r) = RCons name t (rowFromList (ts, r))

ensureNoDuplicateProperties :: [(String, Value)] -> Check ()
ensureNoDuplicateProperties ps = guardWith "Duplicate property names" $ length (nub . map fst $ ps) == length ps

typeConstraints :: M.Map Ident Int -> Value -> Check ([TypeConstraint], Int)
typeConstraints _ v@(NumericLiteral _) = do
  me <- fresh
  return ([TypeConstraint me Number (ValueOrigin v)], me)
typeConstraints _ v@(StringLiteral _) = do
  me <- fresh
  return ([TypeConstraint me String (ValueOrigin v)], me)
typeConstraints _ v@(BooleanLiteral _) = do
  me <- fresh
  return ([TypeConstraint me Boolean (ValueOrigin v)], me)
typeConstraints m v@(ArrayLiteral vals) = do
  all <- mapM (typeConstraints m) vals
  let (cs, ns) = (concatMap fst &&& map snd) all
  me <- fresh
  return (cs ++ zipWith (\n el -> TypeConstraint me (Array $ TUnknown n) (ValueOrigin el)) ns vals, me)
typeConstraints m u@(Unary op val) = do
  (cs, n1) <- typeConstraints m val
  me <- fresh
  return (cs ++ unaryOperatorConstraints u op n1 me, me)
typeConstraints m b@(Binary op left right) = do
  (cs1, n1) <- typeConstraints m left
  (cs2, n2) <- typeConstraints m right
  me <- fresh
  return (cs1 ++ cs2 ++ binaryOperatorConstraints b op n1 n2 me, me)
typeConstraints m v@(ObjectLiteral ps) = do
  ensureNoDuplicateProperties ps
  all <- mapM (typeConstraints m . snd) ps
  let (cs, ns) = (concatMap fst &&& map snd) all
  me <- fresh
  let tys = zipWith (\(name, _) u -> (name, TUnknown u)) ps ns
  return (TypeConstraint me (Object (typesToRow tys)) (ValueOrigin v) : cs, me)
typeConstraints m v@(ObjectUpdate o ps) = do
  ensureNoDuplicateProperties ps
  (cs1, n1) <- typeConstraints m o
  all <- mapM (typeConstraints m . snd) ps
  let (cs2, ns) = (concatMap fst &&& map snd) all
  row <- fresh
  let tys = zipWith (\(name, _) u -> (name, TUnknown u)) ps ns
  return (TypeConstraint n1 (Object (rowFromList (tys, RUnknown row))) (ValueOrigin v) : cs1 ++ cs2, n1)
typeConstraints m v@(Indexer index val) = do
  (cs1, n1) <- typeConstraints m index
  (cs2, n2) <- typeConstraints m val
  me <- fresh
  return (TypeConstraint n1 Number (ValueOrigin index) : TypeConstraint n2 (Array (TUnknown me)) (ValueOrigin v) : cs1 ++ cs2, me)
typeConstraints m v@(Accessor prop val) = do
  (cs, n1) <- typeConstraints m val
  me <- fresh
  rest <- fresh
  return (TypeConstraint n1 (Object (RCons prop (TUnknown me) (RUnknown rest))) (ValueOrigin v) : cs, me)
typeConstraints m v@(Abs args ret) = do
  ns <- replicateM (length args) fresh
  let m' = m `M.union` M.fromList (zip args ns)
  (cs, n') <- typeConstraints m' ret
  me <- fresh
  return (TypeConstraint me (Function (map TUnknown ns) (TUnknown n')) (ValueOrigin v) : cs, me)
typeConstraints m v@(App f xs) = do
  (cs1, n1) <- typeConstraints m f
  all <- mapM (typeConstraints m) xs
  let (cs2, ns) = (concatMap fst &&& map snd) all
  me <- fresh
  return (TypeConstraint n1 (Function (map TUnknown ns) (TUnknown me)) (ValueOrigin v) : cs1 ++ cs2, me)
typeConstraints m v@(Var var@(Qualified mp name)) = do
  case mp of
    ModulePath [] ->
      case M.lookup name m of
        Just u -> do
          me <- fresh
          return ([TypeConstraint u (TUnknown me) (ValueOrigin v)], me)
        Nothing -> lookupGlobal
    _ -> lookupGlobal
  where
  lookupGlobal = do
    env <- getEnv
    modulePath <- checkModulePath `fmap` get
    case M.lookup (qualify modulePath var) (names env) of
      Nothing -> throwError $ show var ++ " is undefined"
      Just (PolyType idents ty, _) -> do
        me <- fresh
        replaced <- replaceVarsWithUnknowns idents ty
        return ([TypeConstraint me replaced (ValueOrigin v)], me)
typeConstraints m (Block ss) = do
  ret <- fresh
  (cs, allCodePathsReturn, _) <- typeConstraintsForBlock m M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
  return (cs, ret)
typeConstraints m v@(Constructor c) = do
  env <- getEnv
  modulePath <- checkModulePath `fmap` get
  case M.lookup (qualify modulePath c) (dataConstructors env) of
    Nothing -> throwError $ "Constructor " ++ show c ++ " is undefined"
    Just (PolyType idents ty) -> do
      me <- fresh
      replaced <- replaceVarsWithUnknowns idents ty
      return ([TypeConstraint me replaced (ValueOrigin v)], me)
typeConstraints m (Case val binders) = do
  (cs1, n1) <- typeConstraints m val
  ret <- fresh
  cs2 <- typeConstraintsForBinders m n1 ret binders
  return (cs1 ++ cs2, ret)
typeConstraints m v@(IfThenElse cond th el) = do
  (cs1, n1) <- typeConstraints m cond
  (cs2, n2) <- typeConstraints m th
  (cs3, n3) <- typeConstraints m el
  return (TypeConstraint n1 Boolean (ValueOrigin cond) : TypeConstraint n2 (TUnknown n3) (ValueOrigin v) : cs1 ++ cs2 ++ cs3, n2)
typeConstraints m v@(TypedValue val poly@(PolyType idents ty)) = do
  kind <- kindOf poly
  guardWith ("Expected type of kind *, was " ++ prettyPrintKind kind) $ kind == Star
  (cs, n1) <- typeConstraints m val
  return (TypeConstraint n1 ty (ValueOrigin v) : cs, n1)

unaryOperatorConstraints :: Value -> UnaryOperator -> Int -> Int -> [TypeConstraint]
unaryOperatorConstraints v Negate val result = [TypeConstraint val Number (ValueOrigin v), TypeConstraint result Number (ValueOrigin v)]
unaryOperatorConstraints v Not val result = [TypeConstraint val Boolean (ValueOrigin v), TypeConstraint result Boolean (ValueOrigin v)]
unaryOperatorConstraints v BitwiseNot val result = [TypeConstraint val Number (ValueOrigin v), TypeConstraint result Number (ValueOrigin v)]

binaryOperatorConstraints :: Value -> BinaryOperator -> Int -> Int -> Int -> [TypeConstraint]
binaryOperatorConstraints v Add = symBinOpConstraints v Number
binaryOperatorConstraints v Subtract = symBinOpConstraints v Number
binaryOperatorConstraints v Multiply = symBinOpConstraints v Number
binaryOperatorConstraints v Divide = symBinOpConstraints v Number
binaryOperatorConstraints v Modulus = symBinOpConstraints v Number
binaryOperatorConstraints v LessThan = asymBinOpConstraints v Number Boolean
binaryOperatorConstraints v LessThanOrEqualTo = asymBinOpConstraints v Number Boolean
binaryOperatorConstraints v GreaterThan = asymBinOpConstraints v Number Boolean
binaryOperatorConstraints v GreaterThanOrEqualTo = asymBinOpConstraints v Number Boolean
binaryOperatorConstraints v BitwiseAnd = symBinOpConstraints v Number
binaryOperatorConstraints v BitwiseOr = symBinOpConstraints v Number
binaryOperatorConstraints v BitwiseXor = symBinOpConstraints v Number
binaryOperatorConstraints v ShiftLeft = symBinOpConstraints v Number
binaryOperatorConstraints v ShiftRight = symBinOpConstraints v Number
binaryOperatorConstraints v ZeroFillShiftRight = symBinOpConstraints v Number
binaryOperatorConstraints v EqualTo = equalityBinOpConstraints v
binaryOperatorConstraints v NotEqualTo = equalityBinOpConstraints v
binaryOperatorConstraints v And = symBinOpConstraints v Boolean
binaryOperatorConstraints v Or = symBinOpConstraints v Boolean
binaryOperatorConstraints v Concat = symBinOpConstraints v String

equalityBinOpConstraints :: Value -> Int -> Int -> Int -> [TypeConstraint]
equalityBinOpConstraints v left right result = [TypeConstraint left (TUnknown right) (ValueOrigin v), TypeConstraint result Boolean (ValueOrigin v)]

symBinOpConstraints :: Value -> Type -> Int -> Int -> Int -> [TypeConstraint]
symBinOpConstraints v ty = asymBinOpConstraints v ty ty

asymBinOpConstraints :: Value -> Type -> Type -> Int -> Int -> Int -> [TypeConstraint]
asymBinOpConstraints v ty res left right result = [TypeConstraint left ty (ValueOrigin v), TypeConstraint right ty (ValueOrigin v), TypeConstraint result res (ValueOrigin v)]

typeConstraintsForBinder :: Int -> Binder -> Check ([TypeConstraint], M.Map Ident Int)
typeConstraintsForBinder _ NullBinder = return ([], M.empty)
typeConstraintsForBinder val b@(StringBinder _) = constantBinder b val String
typeConstraintsForBinder val b@(NumberBinder _) = constantBinder b val Number
typeConstraintsForBinder val b@(BooleanBinder _) = constantBinder b val Boolean
typeConstraintsForBinder val b@(VarBinder name) = do
  me <- fresh
  return ([TypeConstraint me (TUnknown val) (BinderOrigin b)], M.singleton name me)
typeConstraintsForBinder val b@(NullaryBinder ctor) = do
  env <- getEnv
  modulePath <- checkModulePath `fmap` get
  case M.lookup (qualify modulePath ctor) (dataConstructors env) of
    Just (PolyType args ret) -> do
      ret' <- replaceVarsWithUnknowns args ret
      return ([TypeConstraint val ret' (BinderOrigin b)], M.empty)
    _ -> throwError $ "Constructor " ++ show ctor ++ " is not defined"
typeConstraintsForBinder val b@(UnaryBinder ctor binder) = do
  env <- getEnv
  modulePath <- checkModulePath `fmap` get
  case M.lookup (qualify modulePath ctor) (dataConstructors env) of
    Just (PolyType idents f@(Function [_] _)) -> do
      obj <- fresh
      (Function [ty] ret) <- replaceVarsWithUnknowns idents f
      (cs, m1) <- typeConstraintsForBinder obj binder
      return (TypeConstraint val ret (BinderOrigin b) : TypeConstraint obj ty (BinderOrigin b) : cs, m1)
    Just _ -> throwError $ show ctor ++ " is not a unary constructor"
    _ -> throwError $ "Constructor " ++ show ctor ++ " is not defined"
typeConstraintsForBinder val b@(ObjectBinder props) = do
  row <- fresh
  rest <- fresh
  (cs, m1) <- typeConstraintsForProperties row (RUnknown rest) props
  return (TypeConstraint val (Object (RUnknown row)) (BinderOrigin b) : cs, m1)
  where
  typeConstraintsForProperties :: Int -> Row -> [(String, Binder)] -> Check ([TypeConstraint], M.Map Ident Int)
  typeConstraintsForProperties nrow row [] = return ([RowConstraint nrow row (BinderOrigin b)], M.empty)
  typeConstraintsForProperties nrow row ((name, binder):binders) = do
    propTy <- fresh
    (cs1, m1) <- typeConstraintsForBinder propTy binder
    (cs2, m2) <- typeConstraintsForProperties nrow (RCons name (TUnknown propTy) row) binders
    return (cs1 ++ cs2, m1 `M.union` m2)
typeConstraintsForBinder val b@(ArrayBinder binders rest) = do
  el <- fresh
  all <- mapM (typeConstraintsForBinder el) binders
  let (cs1, m1) = (concatMap fst &&& M.unions . map snd) all
  let arrayConstraint = TypeConstraint val (Array (TUnknown el)) (BinderOrigin b)
  case rest of
    Nothing -> return (arrayConstraint : cs1, m1)
    Just binder -> do
      (cs2, m2) <- typeConstraintsForBinder val binder
      return (arrayConstraint : cs1 ++ cs2, m1 `M.union` m2)
typeConstraintsForBinder val b@(NamedBinder name binder) = do
  me <- fresh
  (cs, m) <- typeConstraintsForBinder val binder
  return (TypeConstraint me (TUnknown val) (BinderOrigin b) : cs, M.insert name me m)

typeConstraintsForGuardedBinder :: M.Map Ident Int -> Int -> Binder -> Check ([TypeConstraint], M.Map Ident Int)
typeConstraintsForGuardedBinder m val b@(GuardedBinder cond binder) = do
  (cs1, m1) <- typeConstraintsForBinder val binder
  (cs2, n) <- typeConstraints (m `M.union` m1) cond
  return (TypeConstraint n Boolean (ValueOrigin cond) : cs1 ++ cs2, m1)
typeConstraintsForGuardedBinder m val b = typeConstraintsForBinder val b >>= return

constantBinder :: Binder -> Int -> Type -> Check ([TypeConstraint], M.Map Ident Int)
constantBinder b val ty = return ([TypeConstraint val ty (BinderOrigin b)], M.empty)

typeConstraintsForBinders :: M.Map Ident Int -> Int -> Int -> [(Binder, Value)] -> Check [TypeConstraint]
typeConstraintsForBinders _ _ _ [] = return []
typeConstraintsForBinders m nval ret ((binder, val):bs) = do
  (cs1, m1) <- typeConstraintsForGuardedBinder m nval binder
  (cs2, n2) <- typeConstraints (m `M.union` m1) val
  cs3 <- typeConstraintsForBinders m nval ret bs
  return (TypeConstraint n2 (TUnknown ret) (BinderOrigin binder) : cs1 ++ cs2 ++ cs3)

assignVariable :: Ident -> M.Map Ident Int -> Check ()
assignVariable name m =
  case M.lookup name m of
    Nothing -> return ()
    Just _ -> throwError $ "Variable with name " ++ show name ++ " already exists."

typeConstraintsForStatement :: M.Map Ident Int -> M.Map Ident Int -> Int -> Statement -> Check ([TypeConstraint], Bool, M.Map Ident Int)
typeConstraintsForStatement m mass ret (VariableIntroduction name val) = do
  assignVariable name (m `M.union` mass)
  (cs1, n1) <- typeConstraints m val
  return (cs1, False, M.insert name n1 mass)
typeConstraintsForStatement m mass ret (Assignment ident val) = do
  (cs1, n1) <- typeConstraints m val
  case M.lookup ident mass of
    Nothing -> throwError $ "No local variable with name " ++ show ident
    Just ty ->
     return (TypeConstraint n1 (TUnknown ty) (AssignmentTargetOrigin ident) : cs1, False, mass)
typeConstraintsForStatement m mass ret (While val inner) = do
  (cs1, n1) <- typeConstraints m val
  (cs2, allCodePathsReturn, _) <- typeConstraintsForBlock m mass ret inner
  return (TypeConstraint n1 Boolean (ValueOrigin val) : cs1 ++ cs2, allCodePathsReturn, mass)
typeConstraintsForStatement m mass ret (If ifst) = do
  (cs, allCodePathsReturn) <- typeConstraintsForIfStatement m mass ret ifst
  return (cs, allCodePathsReturn, mass)
typeConstraintsForStatement m mass ret (For ident start end inner) = do
  assignVariable ident (m `M.union` mass)
  (cs1, n1) <- typeConstraints (m `M.union` mass) start
  (cs2, n2) <- typeConstraints (m `M.union` mass) end
  let mass1 = M.insert ident n1 mass
  (cs3, allCodePathsReturn, _) <- typeConstraintsForBlock (m `M.union` mass1) mass1 ret inner
  return (TypeConstraint n1 Number (ValueOrigin start) : TypeConstraint n2 Number (ValueOrigin end) : cs1 ++ cs2 ++ cs3, allCodePathsReturn, mass)
typeConstraintsForStatement m mass ret (ForEach ident vals inner) = do
  assignVariable ident (m `M.union` mass)
  val <- fresh
  (cs1, n1) <- typeConstraints (m `M.union` mass) vals
  let mass1 = M.insert ident val mass
  (cs2, allCodePathsReturn, _) <- typeConstraintsForBlock (m `M.union` mass1) mass1 ret inner
  guardWith "Cannot return from within a foreach block" $ not allCodePathsReturn
  return (TypeConstraint n1 (Array (TUnknown val)) (ValueOrigin vals) : cs1 ++ cs2, False, mass)
typeConstraintsForStatement m mass ret (Return val) = do
  (cs1, n1) <- typeConstraints (m `M.union` mass) val
  return (TypeConstraint n1 (TUnknown ret) (ValueOrigin val) : cs1, True, mass)

typeConstraintsForIfStatement :: M.Map Ident Int -> M.Map Ident Int -> Int -> IfStatement -> Check ([TypeConstraint], Bool)
typeConstraintsForIfStatement m mass ret (IfStatement val thens Nothing) = do
  (cs1, n1) <- typeConstraints m val
  (cs2, _, _) <- typeConstraintsForBlock m mass ret thens
  return (TypeConstraint n1 Boolean (ValueOrigin val) : cs1 ++ cs2, False)
typeConstraintsForIfStatement m mass ret (IfStatement val thens (Just elses)) = do
  (cs1, n1) <- typeConstraints m val
  (cs2, allCodePathsReturn1, _) <- typeConstraintsForBlock m mass ret thens
  (cs3, allCodePathsReturn2) <- typeConstraintsForElseStatement m mass ret elses
  return (TypeConstraint n1 Boolean (ValueOrigin val) : cs1 ++ cs2 ++ cs3, allCodePathsReturn1 && allCodePathsReturn2)

typeConstraintsForElseStatement :: M.Map Ident Int -> M.Map Ident Int -> Int -> ElseStatement -> Check ([TypeConstraint], Bool)
typeConstraintsForElseStatement m mass ret (Else elses) = do
  (cs, allCodePathsReturn, _) <- typeConstraintsForBlock m mass ret elses
  return (cs, allCodePathsReturn)
typeConstraintsForElseStatement m mass ret (ElseIf ifst) = do
  (cs, allCodePathsReturn) <- typeConstraintsForIfStatement m mass ret ifst
  return (cs, allCodePathsReturn)

typeConstraintsForBlock :: M.Map Ident Int -> M.Map Ident Int -> Int -> [Statement] -> Check ([TypeConstraint], Bool, M.Map Ident Int)
typeConstraintsForBlock _ mass _ [] = return ([], False, mass)
typeConstraintsForBlock m mass ret (s:ss) = do
  (cs1, b1, mass1) <- typeConstraintsForStatement (m `M.union` mass) mass ret s
  case (b1, ss) of
    (True, []) -> return (cs1, True, mass1)
    (True, _) -> throwError "Unreachable code"
    (False, ss) -> do
      (cs2, b2, mass2) <- typeConstraintsForBlock m mass1 ret ss
      return (cs1 ++ cs2, b2, mass2)

solveTypeConstraints :: [TypeConstraint] -> TypeSolution -> Check TypeSolution
solveTypeConstraints [] s = return s
solveTypeConstraints all@(TypeConstraint n t o:cs) s = do
  (cs', s') <- rethrow (\err -> "Error in " ++ prettyPrintOrigin o ++ ": " ++ err) $ do
    typeOccursCheck n t
    let s' = let (f, g) = runTypeSolution s
             in TypeSolution (replaceType n t . f, replaceType n t . g)
    cs' <- fmap concat $ mapM (substituteTypeInConstraint n t) cs
    return (cs', s')
  solveTypeConstraints cs' s'
solveTypeConstraints (RowConstraint n r o:cs) s = do
  (cs', s') <- rethrow (\err -> "Error in " ++ prettyPrintOrigin o ++ ": " ++ err) $ do
    rowOccursCheck n r
    let s' = let (f, g) = runTypeSolution s
             in TypeSolution (replaceRow n r . f, replaceRow n r . g)
    cs' <- fmap concat $ mapM (substituteRowInConstraint n r) cs
    return (cs', s')
  solveTypeConstraints cs' s'

substituteTypeInConstraint :: Int -> Type -> TypeConstraint -> Check [TypeConstraint]
substituteTypeInConstraint n s (TypeConstraint m t v)
  | n == m = unifyTypes v s t
  | otherwise = return [TypeConstraint m (replaceType n s t) v]
substituteTypeInConstraint n s (RowConstraint m r v)
  = return [RowConstraint m (replaceType n s r) v]

substituteRowInConstraint :: Int -> Row -> TypeConstraint -> Check [TypeConstraint]
substituteRowInConstraint n r (TypeConstraint m t v)
  = return [TypeConstraint m (replaceRow n r t) v]
substituteRowInConstraint n r (RowConstraint m r1 v)
  | m == n = unifyRows v r r1
  | otherwise = return [RowConstraint m (replaceRow n r r1) v]

unifyTypes :: TypeConstraintOrigin -> Type -> Type -> Check [TypeConstraint]
unifyTypes _ (TUnknown u1) (TUnknown u2) | u1 == u2 = return []
unifyTypes o (TUnknown u) t = do
  typeOccursCheck u t
  return [TypeConstraint u t o]
unifyTypes o t (TUnknown u) = do
  typeOccursCheck u t
  return [TypeConstraint u t o]
unifyTypes o (SaturatedTypeSynonym name1 args1) (SaturatedTypeSynonym name2 args2) | name1 == name2 =
  fmap concat $ zipWithM (unifyTypes o) args1 args2
unifyTypes o (SaturatedTypeSynonym name args) ty = do
  env <- getEnv
  modulePath <- checkModulePath `fmap` get
  case M.lookup (qualify modulePath name) (typeSynonyms env) of
    Just (synArgs, body) -> do
      let m = M.fromList $ zip synArgs args
      let replaced = replaceTypeVars m body
      unifyTypes o replaced ty
    Nothing -> error "Type synonym was not defined"
unifyTypes o ty s@(SaturatedTypeSynonym _ _) = unifyTypes o s ty
unifyTypes _ Number Number = return []
unifyTypes _ String String = return []
unifyTypes _ Boolean Boolean = return []
unifyTypes o (Array s) (Array t) = unifyTypes o s t
unifyTypes o (Object row1) (Object row2) = unifyRows o row1 row2
unifyTypes o (Function args1 ret1) (Function args2 ret2) = do
  guardWith "Function applied to incorrect number of args" $ length args1 == length args2
  cs1 <- fmap concat $ zipWithM (unifyTypes o) args1 args2
  cs2 <- unifyTypes o ret1 ret2
  return $ cs1 ++ cs2
unifyTypes _ (TypeVar v1) (TypeVar v2) | v1 == v2 = return []
unifyTypes _ (TypeConstructor c1) (TypeConstructor c2) = do
  modulePath <- checkModulePath `fmap` get
  guardWith ("Cannot unify " ++ show c1 ++ " with " ++ show c2 ++ ".") (qualify modulePath c1 == qualify modulePath c2)
  return []
unifyTypes o (TypeApp t1 t2) (TypeApp t3 t4) = do
  cs1 <- unifyTypes o t1 t3
  cs2 <- unifyTypes o t2 t4
  return $ cs1 ++ cs2
unifyTypes _ t1 t2 = throwError $ "Cannot unify " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2 ++ "."

unifyRows :: TypeConstraintOrigin -> Row -> Row -> Check [TypeConstraint]
unifyRows o r1 r2 =
  let
    (s1, r1') = rowToList r1
    (s2, r2') = rowToList r2
    int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
    sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
    sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
  in do
    cs1 <- fmap concat $ mapM (uncurry $ unifyTypes o) int
    cs2 <- unifyRows' o sd1 r1' sd2 r2'
    return $ cs1 ++ cs2
  where
  unifyRows' :: TypeConstraintOrigin -> [(String, Type)] -> Row -> [(String, Type)] -> Row -> Check [TypeConstraint]
  unifyRows' o [] (RUnknown u) sd r = do
    rowOccursCheck u r
    return [RowConstraint u (rowFromList (sd, r)) o]
  unifyRows' o sd r [] (RUnknown u) = do
    rowOccursCheck u r
    return [RowConstraint u (rowFromList (sd, r)) o]
  unifyRows' o ns@((name, ty):row) r others (RUnknown u) | not (occursCheck u (ty, row)) = do
    u' <- fresh
    cs <- unifyRows' o row r others (RUnknown u')
    return (RowConstraint u (RCons name ty (RUnknown u')) o : cs)
  unifyRows' _ [] REmpty [] REmpty = return []
  unifyRows' _ [] (RowVar v1) [] (RowVar v2) | v1 == v2 = return []
  unifyRows' _ sd1 r1 sd2 r2 = throwError $ "Cannot unify " ++ prettyPrintRow (rowFromList (sd1, r1)) ++ " with " ++ prettyPrintRow (rowFromList (sd2, r2)) ++ "."
