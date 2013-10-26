-----------------------------------------------------------------------------
--
-- Module      :  PureScript.TypeChecker.Types
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

module PureScript.TypeChecker.Types (
    TypeConstraint(..),
    TypeSolution(..),
    typeOf
) where

import Data.List
import Data.Function
import Data.Data (Data(..))
import Data.Generics (everywhere, everywhereM, everything, mkT, mkM, mkQ, extM, extQ)

import PureScript.Values
import PureScript.Types
import PureScript.Kinds
import PureScript.Names
import PureScript.TypeChecker.Monad
import PureScript.TypeChecker.Kinds
import PureScript.TypeChecker.Synonyms

import Control.Monad.State
import Control.Monad.Error

import Control.Applicative
import Control.Arrow (Kleisli(..), (***), (&&&))
import qualified Control.Category as C

import qualified Data.Map as M

data TypeConstraint
  = TypeConstraint Int Type
  | RowConstraint Int Row deriving Show

newtype TypeSolution = TypeSolution { runTypeSolution :: (Int -> Type, Int -> Row) }

emptyTypeSolution :: TypeSolution
emptyTypeSolution = TypeSolution (TUnknown, RUnknown)

typeOf :: Ident -> Value -> Check PolyType
typeOf name val = do
  (cs, n) <- case val of 
    Abs _ _ -> do
      me <- fresh
      (cs, n) <- typeConstraints (M.singleton name me) val
      return (TypeConstraint me (TUnknown n) : cs, n)
    _ -> typeConstraints M.empty val
  solution <- solveTypeConstraints cs emptyTypeSolution
  let ty = fst (runTypeSolution solution) n
  allUnknownsBecameQuantified cs solution ty
  return $ varIfUnknown ty

allUnknownsBecameQuantified :: [TypeConstraint] -> TypeSolution -> Type -> Check ()
allUnknownsBecameQuantified cs solution ty = do
  let
    typesMentioned = findUnknownTypes ty
    unknownTypes = nub $ flip concatMap cs $ \c -> case c of
      TypeConstraint u t -> u : findUnknownTypes t
      RowConstraint _ r -> findUnknownTypes r
    unsolvedTypes = filter (\n -> TUnknown n == fst (runTypeSolution solution) n) unknownTypes
  guardWith "Unsolved type variable" $ null $ unsolvedTypes \\ typesMentioned
  let
    rowsMentioned = findUnknownRows ty
    unknownRows = nub $ flip concatMap cs $ \c -> case c of
      TypeConstraint _ t -> findUnknownRows t
      RowConstraint u r -> u : findUnknownRows r
    unsolvedRows = filter (\n -> RUnknown n == snd (runTypeSolution solution) n) unknownRows
  guardWith ("Unsolved row variable" ++ show (unsolvedRows \\ rowsMentioned) ++ ", " ++ show cs) $ null $ unsolvedRows \\ rowsMentioned

findUnknownTypes :: (Data d) => d -> [Int]
findUnknownTypes = everything (++) (mkQ [] f)
  where
  f :: Type -> [Int]
  f (TUnknown n) = [n]
  f _ = []

findTypeVars :: (Data d) => d -> [String]
findTypeVars = everything (++) (mkQ [] f)
  where
  f :: Type -> [String]
  f (TypeVar v) = [v]
  f _ = []

findUnknownRows :: (Data d) => d -> [Int]
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
        let name = "t" ++ show (M.size m)
        put $ M.insert n name m
        return $ TypeVar name
      Just name -> return $ TypeVar name
  f t = return t
  g :: Row -> State (M.Map Int String) Row
  g (RUnknown n) = do
    m <- get
    case M.lookup n m of
      Nothing -> do
        let name = "r" ++ show (M.size m)
        put $ M.insert n name m
        return $ RowVar name
      Just name -> return $ RowVar name
  g r = return r

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

replaceType :: (Data d) => Int -> Type -> d -> d
replaceType n t = everywhere (mkT go)
  where
  go (TUnknown m) | m == n = t
  go t = t

replaceRow :: (Data d) => Int -> Row -> d -> d
replaceRow n r = everywhere (mkT go)
  where
  go (RUnknown m) | m == n = r
  go r = r

typeOccursCheck :: Int -> Type -> Check ()
typeOccursCheck u (TUnknown _) = return ()
typeOccursCheck u t = when (occursCheck u t) $ throwError $ "Occurs check failed: " ++ show u ++ " = " ++ show t

rowOccursCheck :: Int -> Row -> Check ()
rowOccursCheck u (RUnknown _) = return ()
rowOccursCheck u r = when (occursCheck u r) $ throwError $ "Occurs check failed: " ++ show u ++ " = " ++ show r

occursCheck :: (Data d) => Int -> d -> Bool
occursCheck u = everything (||) $ flip extQ g $ mkQ False f
  where
  f (TUnknown u') | u' == u = True
  f _ = False
  g (RUnknown u') | u' == u = True
  g _ = False

typeConstraints :: M.Map Ident Int -> Value -> Check ([TypeConstraint], Int)
typeConstraints _ (NumericLiteral _) = do
  me <- fresh
  return ([TypeConstraint me Number], me)
typeConstraints _ (StringLiteral _) = do
  me <- fresh
  return ([TypeConstraint me String], me)
typeConstraints _ (BooleanLiteral _) = do
  me <- fresh
  return ([TypeConstraint me Boolean], me)
typeConstraints m (ArrayLiteral vals) = do
  all <- mapM (typeConstraints m) vals
  let (cs, ns) = (concat . map fst &&& map snd) all
  me <- fresh
  return (cs ++ map (TypeConstraint me . Array . TUnknown) ns, me)
typeConstraints m (Unary op val) = do
  (cs, n1) <- typeConstraints m val
  me <- fresh
  return (cs ++ unaryOperatorConstraints op n1 me, me)
typeConstraints m (Binary op left right) = do
  (cs1, n1) <- typeConstraints m left
  (cs2, n2) <- typeConstraints m right
  me <- fresh
  return (cs1 ++ cs2 ++ binaryOperatorConstraints op n1 n2 me, me)
typeConstraints m (ObjectLiteral ps) = do
  all <- mapM (typeConstraints m . snd) ps
  let (cs, ns) = (concat . map fst &&& map snd) all
  me <- fresh
  let tys = zipWith (\(name, _) u -> (name, TUnknown u)) ps ns
  return ((TypeConstraint me (Object (typesToRow tys))) : cs, me)
  where
    typesToRow [] = REmpty
    typesToRow ((name, ty):tys) = RCons name ty (typesToRow tys)
typeConstraints m (Indexer index val) = do
  (cs1, n1) <- typeConstraints m index
  (cs2, n2) <- typeConstraints m val
  me <- fresh
  return ((TypeConstraint n1 Number) : (TypeConstraint n2 (Array (TUnknown me))) : cs1 ++ cs2, me)
typeConstraints m (Accessor prop val) = do
  (cs, n1) <- typeConstraints m val
  me <- fresh
  rest <- fresh
  return ((TypeConstraint n1 (Object (RCons prop (TUnknown me) (RUnknown rest)))) : cs, me)
typeConstraints m (Abs args ret) = do
  ns <- replicateM (length args) fresh
  let m' = m `M.union` M.fromList (zipWith (,) args ns)
  (cs, n') <- typeConstraints m' ret
  me <- fresh
  return ((TypeConstraint me (Function (map TUnknown ns) (TUnknown n'))) : cs, me)
typeConstraints m (App f xs) = do
  (cs1, n1) <- typeConstraints m f
  all <- mapM (typeConstraints m) xs
  let (cs2, ns) = (concat . map fst &&& map snd) all
  me <- fresh
  return ((TypeConstraint n1 (Function (map TUnknown ns) (TUnknown me))) : cs1 ++ cs2, me)
typeConstraints m (Var var) =
  case M.lookup var m of
    Nothing -> do
      env <- getEnv
      case M.lookup var (names env) of
        Nothing -> throwError $ show var ++ " is undefined"
        Just (PolyType idents ty, _) -> do
          me <- fresh
          replaced <- replaceVarsWithUnknowns idents ty
          return ([TypeConstraint me replaced], me)
    Just u -> do
      me <- fresh
      return ([TypeConstraint u (TUnknown me)], me)
typeConstraints m (Block ss) = do
  ret <- fresh
  (cs, allCodePathsReturn, _) <- typeConstraintsForBlock m M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
  return (cs, ret)
typeConstraints m (Constructor c) = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError $ "Constructor " ++ c ++ " is undefined"
    Just (PolyType idents ty) -> do
      me <- fresh
      replaced <- replaceVarsWithUnknowns idents ty
      return ([TypeConstraint me replaced], me)
typeConstraints m (Case val binders) = do
  (cs1, n1) <- typeConstraints m val
  ret <- fresh
  cs2 <- typeConstraintsForBinders m n1 ret binders
  return (cs1 ++ cs2, ret)
typeConstraints m (TypedValue val poly@(PolyType idents ty)) = do
  kind <- kindOf poly
  guardWith ("Expected type of kind *, was " ++ show kind) $ kind == Star
  desugared <- replaceAllTypeSynonyms ty
  (cs, n1) <- typeConstraints m val
  return (TypeConstraint n1 desugared : cs, n1)

replaceAllTypeSynonyms :: Type -> Check Type
replaceAllTypeSynonyms ty = do
  env <- getEnv
  go (M.toList $ typeSynonyms env) ty
  where
  go [] ty = return ty
  go ((name, (args, repl)):rest) ty = do
    ty' <- either throwError return (substituteTypeSynonym name args repl ty)
    go rest ty'

unaryOperatorConstraints :: UnaryOperator -> Int -> Int -> [TypeConstraint]
unaryOperatorConstraints Negate val result = [TypeConstraint val Number, TypeConstraint result Number]
unaryOperatorConstraints Not val result = [TypeConstraint val Boolean, TypeConstraint result Boolean]
unaryOperatorConstraints BitwiseNot val result = [TypeConstraint val Number, TypeConstraint result Number]

binaryOperatorConstraints :: BinaryOperator -> Int -> Int -> Int -> [TypeConstraint]
binaryOperatorConstraints Add = symBinOpConstraints Number
binaryOperatorConstraints Subtract = symBinOpConstraints Number
binaryOperatorConstraints Multiply = symBinOpConstraints Number
binaryOperatorConstraints Divide = symBinOpConstraints Number
binaryOperatorConstraints Modulus = symBinOpConstraints Number
binaryOperatorConstraints LessThan = asymBinOpConstraints Number Boolean
binaryOperatorConstraints LessThanOrEqualTo = asymBinOpConstraints Number Boolean
binaryOperatorConstraints GreaterThan = asymBinOpConstraints Number Boolean
binaryOperatorConstraints GreaterThanOrEqualTo = asymBinOpConstraints Number Boolean
binaryOperatorConstraints BitwiseAnd = symBinOpConstraints Number
binaryOperatorConstraints BitwiseOr = symBinOpConstraints Number
binaryOperatorConstraints BitwiseXor = symBinOpConstraints Number
binaryOperatorConstraints ShiftLeft = symBinOpConstraints Number
binaryOperatorConstraints ShiftRight = symBinOpConstraints Number
binaryOperatorConstraints ZeroFillShiftRight = symBinOpConstraints Number
binaryOperatorConstraints EqualTo = equalityBinOpConstraints
binaryOperatorConstraints NotEqualTo = equalityBinOpConstraints
binaryOperatorConstraints And = symBinOpConstraints Boolean
binaryOperatorConstraints Or = symBinOpConstraints Boolean
binaryOperatorConstraints Concat = symBinOpConstraints String

equalityBinOpConstraints :: Int -> Int -> Int -> [TypeConstraint]
equalityBinOpConstraints left right result = [TypeConstraint left (TUnknown right), TypeConstraint result Boolean]

symBinOpConstraints :: Type -> Int -> Int -> Int -> [TypeConstraint]
symBinOpConstraints ty left right result = asymBinOpConstraints ty ty left right result

asymBinOpConstraints :: Type -> Type -> Int -> Int -> Int -> [TypeConstraint]
asymBinOpConstraints ty res left right result = [TypeConstraint left ty, TypeConstraint right ty, TypeConstraint result res]

typeConstraintsForBinder :: Int -> Binder -> Check ([TypeConstraint], M.Map Ident Int)
typeConstraintsForBinder _ NullBinder = return ([], M.empty)
typeConstraintsForBinder val (StringBinder _) = constantBinder val String
typeConstraintsForBinder val (NumberBinder _) = constantBinder val Number
typeConstraintsForBinder val (BooleanBinder _) = constantBinder val Boolean
typeConstraintsForBinder val (VarBinder name) = do
  me <- fresh
  return ([TypeConstraint me (TUnknown val)], M.singleton name me)
typeConstraintsForBinder val (NullaryBinder ctor) = do
  env <- getEnv
  case M.lookup ctor (dataConstructors env) of
    Just (PolyType args ret) -> do
      ret' <- replaceVarsWithUnknowns args ret
      return ([TypeConstraint val ret'], M.empty)
    _ -> throwError $ "Constructor " ++ ctor ++ " is not defined"
typeConstraintsForBinder val (UnaryBinder ctor binder) = do
  env <- getEnv
  case M.lookup ctor (dataConstructors env) of
    Just (PolyType idents f@(Function [_] _)) -> do
      obj <- fresh
      (Function [ty] ret) <- replaceVarsWithUnknowns idents f
      (cs, m1) <- typeConstraintsForBinder obj binder
      return ((TypeConstraint val ret) : (TypeConstraint obj ty) : cs, m1)
    Just _ -> throwError $ ctor ++ " is not a unary constructor"
    _ -> throwError $ "Constructor " ++ ctor ++ " is not defined"
typeConstraintsForBinder val (ObjectBinder props) = do
  row <- fresh
  rest <- fresh
  (cs, m1) <- typeConstraintsForProperties row (RUnknown rest) props
  return ((TypeConstraint val (Object (RUnknown row))) : cs, m1)
  where
  typeConstraintsForProperties :: Int -> Row -> [(String, Binder)] -> Check ([TypeConstraint], M.Map Ident Int)
  typeConstraintsForProperties nrow row [] = return ([RowConstraint nrow row], M.empty)
  typeConstraintsForProperties nrow row ((name, binder):binders) = do
    propTy <- fresh
    (cs1, m1) <- typeConstraintsForBinder propTy binder
    (cs2, m2) <- typeConstraintsForProperties nrow (RCons name (TUnknown propTy) row) binders
    return (cs1 ++ cs2, M.union m1 m2)
typeConstraintsForBinder val (ArrayBinder binders rest) = do
  el <- fresh
  all <- mapM (typeConstraintsForBinder el) binders
  let (cs1, m1) = (concat . map fst &&& M.unions . map snd) all
  let arrayConstraint = TypeConstraint val (Array (TUnknown el))
  case rest of
    Nothing -> return (arrayConstraint : cs1, m1)
    Just binder -> do
      (cs2, m2) <- typeConstraintsForBinder val binder
      return (arrayConstraint : cs1 ++ cs2, M.union m1 m2)
typeConstraintsForBinder val (NamedBinder name binder) = do
  me <- fresh
  (cs, m) <- typeConstraintsForBinder val binder
  return (TypeConstraint me (TUnknown val) : cs, M.insert name me m)
typeConstraintsForBinder val (GuardedBinder cond binder) = do
  (cs1, m) <- typeConstraintsForBinder val binder
  (cs2, n) <- typeConstraints m cond
  return (TypeConstraint n Boolean : cs1 ++ cs2, m)

constantBinder :: Int -> Type -> Check ([TypeConstraint], M.Map Ident Int)
constantBinder val ty = return ([TypeConstraint val ty], M.empty)

typeConstraintsForBinders :: M.Map Ident Int -> Int -> Int -> [(Binder, Value)] -> Check [TypeConstraint]
typeConstraintsForBinders _ _ _ [] = return []
typeConstraintsForBinders m nval ret ((binder, val):bs) = do
  (cs1, m1) <- typeConstraintsForBinder nval binder
  (cs2, n2) <- typeConstraints (M.union m m1) val
  cs3 <- typeConstraintsForBinders m nval ret bs
  return ((TypeConstraint n2 (TUnknown ret)) : cs1 ++ cs2 ++ cs3)

assignVariable :: Ident -> M.Map Ident Int -> Check ()
assignVariable name m =
  case M.lookup name m of
    Nothing -> return ()
    Just _ -> throwError $ "Variable with name " ++ show name ++ " already exists."

typeConstraintsForStatement :: M.Map Ident Int -> M.Map Ident Int -> Int -> Statement -> Check ([TypeConstraint], Bool, M.Map Ident Int)
typeConstraintsForStatement m mass ret (VariableIntroduction name val) = do
  assignVariable name (m `M.union` mass)
  (cs1, n1) <- typeConstraints m val
  return $ (cs1, False, M.insert name n1 mass)
typeConstraintsForStatement m mass ret (Assignment tgt val) = do
  (cs1, n1) <- typeConstraints m val
  cs2 <- typeConstraintsForAssignmentTarget m mass n1 tgt
  return (cs1 ++ cs2, False, mass)
typeConstraintsForStatement m mass ret (While val inner) = do
  (cs1, n1) <- typeConstraints m val
  (cs2, allCodePathsReturn, _) <- typeConstraintsForBlock m mass ret inner
  return $ ((TypeConstraint n1 Boolean) : cs1 ++ cs2, allCodePathsReturn, mass)
typeConstraintsForStatement m mass ret (If ifst) = do
  (cs, allCodePathsReturn) <- typeConstraintsForIfStatement m mass ret ifst
  return $ (cs, allCodePathsReturn, mass)
typeConstraintsForStatement m mass ret (For ident start end inner) = do
  assignVariable ident (m `M.union` mass)
  (cs1, n1) <- typeConstraints (m `M.union` mass) start
  (cs2, n2) <- typeConstraints (m `M.union` mass) end
  let mass1 = M.insert ident n1 mass
  (cs3, allCodePathsReturn, _) <- typeConstraintsForBlock (m `M.union` mass1) mass1 ret inner
  return $ ((TypeConstraint n1 Number) : (TypeConstraint n2 Number) : cs1 ++ cs2 ++ cs3, allCodePathsReturn, mass)
typeConstraintsForStatement m mass ret (ForEach ident vals inner) = do
  assignVariable ident (m `M.union` mass)
  val <- fresh
  (cs1, n1) <- typeConstraints (m `M.union` mass) vals
  let mass1 = M.insert ident val mass
  (cs2, allCodePathsReturn, _) <- typeConstraintsForBlock (m `M.union` mass1) mass1 ret inner
  guardWith "Cannot return from within a foreach block" $ not allCodePathsReturn
  return $ ((TypeConstraint n1 (Array (TUnknown val))) : cs1 ++ cs2, False, mass)
typeConstraintsForStatement m mass ret (Return val) = do
  (cs1, n1) <- typeConstraints (m `M.union` mass) val
  return ((TypeConstraint n1 (TUnknown ret)) : cs1, True, mass)

typeConstraintsForIfStatement :: M.Map Ident Int -> M.Map Ident Int -> Int -> IfStatement -> Check ([TypeConstraint], Bool)
typeConstraintsForIfStatement m mass ret (IfStatement val thens Nothing) = do
  (cs1, n1) <- typeConstraints m val
  (cs2, allCodePathsReturn, _) <- typeConstraintsForBlock m mass ret thens
  return ((TypeConstraint n1 Boolean) : cs1 ++ cs2, allCodePathsReturn)
typeConstraintsForIfStatement m mass ret (IfStatement val thens (Just elses)) = do
  (cs1, n1) <- typeConstraints m val
  (cs2, allCodePathsReturn1, _) <- typeConstraintsForBlock m mass ret thens
  (cs3, allCodePathsReturn2) <- typeConstraintsForElseStatement m mass ret elses
  return ((TypeConstraint n1 Boolean) : cs1 ++ cs2 ++ cs3, allCodePathsReturn1 && allCodePathsReturn2)

typeConstraintsForElseStatement :: M.Map Ident Int -> M.Map Ident Int -> Int -> ElseStatement -> Check ([TypeConstraint], Bool)
typeConstraintsForElseStatement m mass ret (Else elses) = do
  (cs, allCodePathsReturn, _) <- typeConstraintsForBlock m mass ret elses
  return (cs, allCodePathsReturn)
typeConstraintsForElseStatement m mass ret (ElseIf ifst) = do
  (cs, allCodePathsReturn) <- typeConstraintsForIfStatement m mass ret ifst
  return (cs, allCodePathsReturn)

typeConstraintsForAssignmentTarget :: M.Map Ident Int -> M.Map Ident Int -> Int -> AssignmentTarget -> Check [TypeConstraint]
typeConstraintsForAssignmentTarget m mass nval (AssignVariable var) =
  case M.lookup var mass of
    Nothing -> throwError $ "No local variable with name " ++ show var
    Just ty -> do
     return [TypeConstraint nval (TUnknown ty)]
typeConstraintsForAssignmentTarget m mass nval (AssignArrayIndex index tgt) = do
  arr <- fresh
  (cs1, n1) <- typeConstraints (m `M.union` mass) index
  cs2 <- typeConstraintsForAssignmentTarget m mass arr tgt
  return $ (TypeConstraint arr (Array (TUnknown nval))) : (TypeConstraint n1 Number) : cs1 ++ cs2
typeConstraintsForAssignmentTarget m mass nval (AssignObjectProperty prop tgt) =  do
  obj <- fresh
  row <- fresh
  cs <- typeConstraintsForAssignmentTarget m mass obj tgt
  return $ (TypeConstraint obj (Object (RCons prop (TUnknown nval) $ RUnknown row))) : cs

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
solveTypeConstraints all@(TypeConstraint n t:cs) s = do
  typeOccursCheck n t
  let s' = let (f, g) = runTypeSolution s
           in TypeSolution (replaceType n t . f, replaceType n t . g)
  cs' <- fmap concat $ mapM (substituteTypeInConstraint n t) cs
  solveTypeConstraints cs' s'
solveTypeConstraints (RowConstraint n r:cs) s = do
  rowOccursCheck n r
  let s' = let (f, g) = runTypeSolution s
           in TypeSolution $ (replaceRow n r . f, replaceRow n r . g)
  cs' <- fmap concat $ mapM (substituteRowInConstraint n r) cs
  solveTypeConstraints cs' s'

substituteTypeInConstraint :: Int -> Type -> TypeConstraint -> Check [TypeConstraint]
substituteTypeInConstraint n s (TypeConstraint m t)
  | n == m = unifyTypes s t
  | otherwise = return [TypeConstraint m (replaceType n s t)]
substituteTypeInConstraint n s (RowConstraint m r)
  = return [RowConstraint m (replaceType n s r)]

substituteRowInConstraint :: Int -> Row -> TypeConstraint -> Check [TypeConstraint]
substituteRowInConstraint n r (TypeConstraint m t)
  = return [TypeConstraint m (replaceRow n r t)]
substituteRowInConstraint n r (RowConstraint m r1)
  | m == n = unifyRows r r1
  | otherwise = return [RowConstraint m (replaceRow n r r1)]

unifyTypes :: Type -> Type -> Check [TypeConstraint]
unifyTypes (TUnknown u1) (TUnknown u2) | u1 == u2 = return []
unifyTypes (TUnknown u) t = do
  typeOccursCheck u t
  return [TypeConstraint u t]
unifyTypes t (TUnknown u) = do
  typeOccursCheck u t
  return [TypeConstraint u t]
unifyTypes Number Number = return []
unifyTypes String String = return []
unifyTypes Boolean Boolean = return []
unifyTypes (Array s) (Array t) = unifyTypes s t
unifyTypes (Object row1) (Object row2) = unifyRows row1 row2
unifyTypes (Function args1 ret1) (Function args2 ret2) = do
  guardWith "Function applied to incorrect number of args" $ length args1 == length args2
  cs1 <- fmap concat $ zipWithM unifyTypes args1 args2
  cs2 <- unifyTypes ret1 ret2
  return $ cs1 ++ cs2
unifyTypes (TypeVar v1) (TypeVar v2) | v1 == v2 = return []
unifyTypes (TypeConstructor c1) (TypeConstructor c2) | c1 == c2 = return []
unifyTypes (TypeVar v1) (TypeVar v2) = return []
unifyTypes (TypeApp t1 t2) (TypeApp t3 t4) = do
  cs1 <- unifyTypes t1 t3
  cs2 <- unifyTypes t2 t4
  return $ cs1 ++ cs2
unifyTypes t1 t2 = throwError $ "Cannot unify " ++ show t1 ++ " with " ++ show t2 ++ "."

unifyRows :: Row -> Row -> Check [TypeConstraint]
unifyRows r1 r2 =
  let
    (s1, r1') = toList r1
    (s2, r2') = toList r2
    int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
    sd1 = [ (name, t1) | (name, t1) <- s1, not (elem name (map fst s2)) ]
    sd2 = [ (name, t2) | (name, t2) <- s2, not (elem name (map fst s1)) ]
  in rethrow (\e -> "Cannot unify " ++ show r1 ++ " with " ++ show r2 ++ ": " ++ e) $ do
    cs1 <- fmap concat $ mapM (uncurry unifyTypes) int
    cs2 <- unifyRows' sd1 r1' sd2 r2'
    return $ cs1 ++ cs2
  where
  unifyRows' :: [(String, Type)] -> Row -> [(String, Type)] -> Row -> Check [TypeConstraint]
  unifyRows' [] (RUnknown u) sd r = do
    rowOccursCheck u r
    return [RowConstraint u (fromList (sd, r))]
  unifyRows' sd r [] (RUnknown u) = do
    rowOccursCheck u r
    return [RowConstraint u (fromList (sd, r))]
  unifyRows' ((name, ty):row) r others (RUnknown u) = do
    u' <- fresh
    cs <- unifyRows' row r others (RUnknown u')
    return (RowConstraint u (RCons name ty (RUnknown u')) : cs)
  unifyRows' [] REmpty [] REmpty = return []
  unifyRows' sd1 r1 sd2 r2 = throwError $ "Cannot unify " ++ show (fromList (sd1, r1)) ++ " with " ++ show (fromList (sd2, r2)) ++ "."
  toList :: Row -> ([(String, Type)], Row)
  toList (RCons name ty row) = let (tys, rest) = toList row
                               in ((name, ty):tys, rest)
  toList r = ([], r)
  fromList :: ([(String, Type)], Row) -> Row
  fromList ([], r) = r
  fromList ((name, t):ts, r) = RCons name t (fromList (ts, r))
