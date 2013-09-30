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

import PureScript.Values
import PureScript.Types
import PureScript.TypeChecker.Monad

import Control.Monad.State
import Control.Monad.Error

import Control.Applicative
import Control.Arrow (Kleisli(..), (***))
import qualified Control.Category as C

import qualified Data.Map as M

data TypeConstraint
  = TypeConstraint Int Type
  | RowConstraint Int Row deriving Show

newtype TypeSolution = TypeSolution { runTypeSolution :: (Int -> Type, Int -> Row) }

emptyTypeSolution :: TypeSolution
emptyTypeSolution = TypeSolution (TUnknown, RUnknown)

typeOf :: Value -> Check Type
typeOf val = do
  (cs, n, _) <- typeConstraints 0 M.empty val
  solution <- solveTypeConstraints cs emptyTypeSolution
  return $ varIfUnknown $ fst (runTypeSolution solution) n

varIfUnknown :: Type -> Type
varIfUnknown = flip evalState M.empty . varIfUnknown'
  where
  varIfUnknown' :: Type -> State (M.Map Int String) Type
  varIfUnknown' (TUnknown n) = do
    m <- get
    case M.lookup n m of
      Nothing -> do
        let name = "t" ++ show (M.size m)
        put $ M.insert n name m
        return $ TypeVar name
      Just name -> return $ TypeVar name
  varIfUnknown' (Array t) = Array <$> varIfUnknown' t
  varIfUnknown' (Object row) = Object <$> varIfUnknown'' row
  varIfUnknown' (Function args ret) = Function <$> mapM varIfUnknown' args <*> varIfUnknown' ret
  varIfUnknown' (TypeApp t1 t2) = TypeApp <$> varIfUnknown' t1 <*> varIfUnknown' t2
  varIfUnknown' t = return t
  varIfUnknown'' :: Row -> State (M.Map Int String) Row
  varIfUnknown'' (RUnknown n) = do
    m <- get
    case M.lookup n m of
      Nothing -> do
        let name = "r" ++ show (M.size m)
        put $ M.insert n name m
        return $ RowVar name
      Just name -> return $ RowVar name
  varIfUnknown'' (RCons name ty row) = RCons name ty <$> varIfUnknown'' row
  varIfUnknown'' r = return r

replaceVarsWithUnknowns :: Int -> Type -> (Type, Int)
replaceVarsWithUnknowns n = (id *** fst) . flip runState (n, M.empty) . replaceVarsWithUnknowns'
  where
  replaceVarsWithUnknowns' :: Type -> State (Int, M.Map String Int) Type
  replaceVarsWithUnknowns' (Array t) = Array <$> replaceVarsWithUnknowns' t
  replaceVarsWithUnknowns' (Object row) = Object <$> replaceVarsWithUnknowns'' row
  replaceVarsWithUnknowns' (Function args ret) = Function <$> mapM replaceVarsWithUnknowns' args <*> replaceVarsWithUnknowns' ret
  replaceVarsWithUnknowns' (TypeApp t1 t2) = TypeApp <$> replaceVarsWithUnknowns' t1 <*> replaceVarsWithUnknowns' t2
  replaceVarsWithUnknowns' (TypeVar var) = do
    (n, m) <- get
    case M.lookup var m of
      Nothing -> do
        put (n + 1, M.insert var n m)
        return $ TUnknown n
      Just u -> return $ TUnknown u
  replaceVarsWithUnknowns' t = return t
  replaceVarsWithUnknowns'' :: Row -> State (Int, M.Map String Int) Row
  replaceVarsWithUnknowns'' (RowVar var) = do
    (n, m) <- get
    case M.lookup var m of
      Nothing -> do
        put (n + 1, M.insert var n m)
        return $ RUnknown n
      Just u -> return $ RUnknown u
  replaceVarsWithUnknowns'' (RCons name ty row) = RCons name <$> replaceVarsWithUnknowns' ty <*> replaceVarsWithUnknowns'' row
  replaceVarsWithUnknowns'' r = return r

typeConstraints :: Int -> M.Map String Int -> Value -> Check ([TypeConstraint], Int, Int)
typeConstraints n _ (NumericLiteral _) = return ([TypeConstraint n Number], n, n)
typeConstraints n _ (StringLiteral _) = return ([TypeConstraint n String], n, n)
typeConstraints n _ (BooleanLiteral _) = return ([TypeConstraint n Boolean], n, n)
typeConstraints n m (ArrayLiteral vals) = do
  (cs, ns, max1) <- typeConstraintsAll n m vals
  let me = max1 + 1
  return (cs ++ map (TypeConstraint me . Array . TUnknown) ns, me, me)
typeConstraints n m (Unary op val) = do
  (cs, n1, max1) <- typeConstraints n m val
  let me = max1 + 1
  return (cs ++ unaryOperatorConstraints op n1 me, me, me)
typeConstraints n m (Binary op left right) = do
  (cs1, n1, max1) <- typeConstraints n m left
  (cs2, n2, max2) <- typeConstraints (max1 + 1) m right
  let me = max1 + 1
  return (cs1 ++ cs2 ++ binaryOperatorConstraints op n1 n2 me, me, me)
typeConstraints n m (ObjectLiteral ps) = do
  (cs, ns, max1) <- typeConstraintsAll n m (map snd ps)
  let me = max1 + 1
  let tys = zipWith (\(name, _) u -> (name, TUnknown u)) ps ns
  return ((TypeConstraint me (Object (typesToRow tys))) : cs, me, me)
  where
    typesToRow [] = REmpty
    typesToRow ((name, ty):tys) = RCons name ty (typesToRow tys)
typeConstraints n m (Accessor prop val) = do
  (cs, n1, max1) <- typeConstraints n m val
  let me = max1 + 1
  let rest = max1 + 2
  return ((TypeConstraint n1 (Object (RCons prop (TUnknown me) (RUnknown rest)))) : cs, me, rest)
typeConstraints n m (Abs args ret) = do
  let ns = [n..n + length args - 1]
  let next = n + length args
  let m' = m `M.union` M.fromList (zipWith (,) args ns)
  (cs, n', max1) <- typeConstraints next m' ret
  let me = max1 + 1
  return ((TypeConstraint me (Function (map TUnknown ns) (TUnknown n'))) : cs, me, me)
typeConstraints n m (App f xs) = do
  (cs1, n1, max1) <- typeConstraints n m f
  (cs2, ns, max2) <- typeConstraintsAll (max1 + 1) m xs
  let me = max2 + 1
  return ((TypeConstraint n1 (Function (map TUnknown ns) (TUnknown me))) : cs1 ++ cs2, me, me)
typeConstraints n m (Var var) =
  case M.lookup var m of
    Nothing -> do
      env <- get
      case M.lookup var (names env) of
        Nothing -> throwError $ var ++ " is undefined"
        Just ty -> let (replaced, max1) = replaceVarsWithUnknowns (n + 1) ty
                   in return ([TypeConstraint n replaced], n, max1)
    Just u -> return ([TypeConstraint u (TUnknown n)], n, n)
typeConstraints n m (Block ss) = do
  let ret = n + 1
  (cs, allCodePathsReturn, max1, _) <- typeConstraintsForBlock (ret + 1) m M.empty ret ss
  guardWith "Block is missing a return statement" allCodePathsReturn
  return (cs, ret, max1)
typeConstraints n m (Constructor c) = do
  env <- get
  case M.lookup c (names env) of
    Nothing -> throwError $ c ++ " is undefined"
    Just ty -> let (replaced, max1) = replaceVarsWithUnknowns (n + 1) ty
               in return ([TypeConstraint n replaced], n, max1)
typeConstraints n m (Case val binders) = do
  (cs1, n1, max1) <- typeConstraints n m val
  let ret = max1 + 1
  (cs2, max2) <- typeConstraintsForBinders (ret + 1) m n1 ret binders
  return (cs1 ++ cs2, ret, max2)
typeConstraints n m (TypedValue val ty) = do
  (cs, n1, max1) <- typeConstraints n m val
  return ([TypeConstraint n1 ty], n1, max1)
  -- TODO: Kind-check the type

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
binaryOperatorConstraints LessThan = symBinOpConstraints Number
binaryOperatorConstraints LessThanOrEqualTo = symBinOpConstraints Number
binaryOperatorConstraints GreaterThan = symBinOpConstraints Number
binaryOperatorConstraints GreaterThanOrEqualTo = symBinOpConstraints Number
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
symBinOpConstraints ty left right result = [TypeConstraint left ty, TypeConstraint right ty, TypeConstraint result ty]

typeConstraintsForBinder :: Int -> M.Map String Int -> Int -> Binder -> Check ([TypeConstraint], Int, M.Map String Int)
typeConstraintsForBinder n m val (VarBinder name) =
  return ([TypeConstraint n (TUnknown val)], n, M.insert name n m)
typeConstraintsForBinder n m val (ConstructorBinder ctor binder) = do
  env <- get
  case M.lookup ctor (names env) of
    Just f@(Function [ty] ret) -> do
      let obj = n
      let (Function [ty'] ret', max1) = replaceVarsWithUnknowns n f
      (cs, max2, m1) <- typeConstraintsForBinder (max1 + 1) m obj binder
      return ((TypeConstraint val ret') : (TypeConstraint obj ty') : cs, max2, m1)
    Just _ -> throwError $ ctor ++ " is not a constructor"
    _ -> throwError $ "Constructor " ++ ctor ++ " is not defined"
typeConstraintsForBinder n m val (ObjectBinder props) = do undefined

typeConstraintsForBinders :: Int -> M.Map String Int -> Int -> Int -> [(Binder, Value)] -> Check ([TypeConstraint], Int)
typeConstraintsForBinders n _ _ _ [] = return ([], n)
typeConstraintsForBinders n m nval ret ((binder, val):bs) = do
  (cs1, max1, m1) <- typeConstraintsForBinder n m nval binder
  (cs2, n2, max2) <- typeConstraints (max1 + 1) m1 val
  (cs3, max2) <- typeConstraintsForBinders (max2 + 1) m nval ret bs
  return ((TypeConstraint n2 (TUnknown ret)) : cs1 ++ cs2 ++ cs3, max2)

typeConstraintsForStatement :: Int -> M.Map String Int -> M.Map String Int -> Int -> Statement -> Check ([TypeConstraint], Bool, Int, M.Map String Int)
typeConstraintsForStatement n m mass ret (VariableIntroduction name val) = do
  case M.lookup name (m `M.union` mass) of
    Nothing -> do
      (cs1, n1, max1) <- typeConstraints n m val
      return $ (cs1, False, max1, M.insert name n1 mass)
    Just ty -> throwError $ "Variable with name " ++ name ++ " already exists."
typeConstraintsForStatement n m mass ret (Assignment name val) = do
  case M.lookup name (m `M.union` mass) of
    Nothing -> throwError $ "No local variable with name " ++ name
    Just ty -> do
      (cs1, n1, max1) <- typeConstraints n m val
      return ((TypeConstraint n1 (TUnknown ty)) : cs1, False, max1, mass)
typeConstraintsForStatement n m mass ret (While val inner) = do
  (cs1, n1, max1) <- typeConstraints n m val
  (cs2, allCodePathsReturn, max2, _) <- typeConstraintsForBlock (max1 + 1) m mass ret inner
  return $ ((TypeConstraint n1 Boolean) : cs1 ++ cs2, allCodePathsReturn, max2, mass)
typeConstraintsForStatement n m mass ret (IfThenElse val thens Nothing) = do
  (cs1, n1, max1) <- typeConstraints n m val
  (cs2, allCodePathsReturn, max2, _) <- typeConstraintsForBlock (max1 + 1) m mass ret thens
  return $ ((TypeConstraint n1 Boolean) : cs1 ++ cs2, allCodePathsReturn, max2, mass)
typeConstraintsForStatement n m mass ret (IfThenElse val thens (Just elses)) = do
  (cs1, n1, max1) <- typeConstraints n m val
  (cs2, allCodePathsReturn1, max2, _) <- typeConstraintsForBlock (max1 + 1) m mass ret thens
  (cs3, allCodePathsReturn2, max3, _) <- typeConstraintsForBlock (max2 + 1) m mass ret elses
  return $ ((TypeConstraint n1 Boolean) : cs1 ++ cs2 ++ cs3, allCodePathsReturn1 && allCodePathsReturn2, max3, mass)
typeConstraintsForStatement n m mass ret (For (init, cond, cont) inner) = do
  (cs1, b1, max1, mass1) <- typeConstraintsForStatement n m mass ret init
  guardWith "Cannot return from inside for" $ not b1
  (cs2, n1, max2) <- typeConstraints (max1 + 1) (m `M.union` mass1) cond
  (cs3, b2, max3, mass2) <- typeConstraintsForStatement (max2 + 1) (m `M.union` mass1) mass1 ret cont
  guardWith "Cannot return from inside for" $ not b2
  (cs4, allCodePathsReturn, max4, _) <- typeConstraintsForBlock (max3 + 1) (m `M.union` mass2) mass2 ret inner
  return $ ((TypeConstraint n1 Boolean) : cs1 ++ cs2 ++ cs3 ++ cs4, allCodePathsReturn, max4, mass)
typeConstraintsForStatement n m mass ret (Return val) = do
  (cs1, n1, max1) <- typeConstraints n m val
  return ((TypeConstraint n1 (TUnknown ret)) : cs1, True, max1, mass)

typeConstraintsForBlock :: Int -> M.Map String Int -> M.Map String Int -> Int -> [Statement] -> Check ([TypeConstraint], Bool, Int, M.Map String Int)
typeConstraintsForBlock n _ mass _ [] = return ([], False, n, mass)
typeConstraintsForBlock n m mass ret (s:ss) = do
  (cs1, b1, max1, mass1) <- typeConstraintsForStatement n (m `M.union` mass) mass ret s
  case (b1, ss) of
    (True, []) -> return (cs1, True, max1, mass1)
    (True, _) -> throwError "Unreachable code"
    (False, ss) -> do
      (cs2, b2, max2, mass2) <- typeConstraintsForBlock (max1 + 1) m mass1 ret ss
      return (cs1 ++ cs2, b2, max2, mass2)

typeConstraintsAll :: Int -> M.Map String Int -> [Value] -> Check ([TypeConstraint], [Int], Int)
typeConstraintsAll n _ [] = return ([], [], n)
typeConstraintsAll n m (t:ts) = do
  (cs, n', max1) <- typeConstraints n m t
  (cs', ns, max2) <- typeConstraintsAll (max1 + 1) m ts
  return (cs ++ cs', n':ns, max2)

solveTypeConstraints :: [TypeConstraint] -> TypeSolution -> Check TypeSolution
solveTypeConstraints [] s = return s
solveTypeConstraints (TypeConstraint n t:cs) s = do
  guardWith "Occurs check failed" $ not $ typeOccursCheck n t
  let s' = let (f, g) = runTypeSolution s
           in TypeSolution (replaceTypeInType n t . f, replaceTypeInRow n t . g)
  cs' <- fmap concat $ mapM (substituteTypeInConstraint n t) cs
  solveTypeConstraints cs' s'
solveTypeConstraints (RowConstraint n r:cs) s = do
  guardWith "Occurs check failed" $ not $ rowOccursCheck n r
  let s' = let (f, g) = runTypeSolution s
           in TypeSolution $ (replaceRowInType n r . f, replaceRowInRow n r . g)
  cs' <- fmap concat $ mapM (substituteRowInConstraint n r) cs
  solveTypeConstraints cs' s'

substituteTypeInConstraint :: Int -> Type -> TypeConstraint -> Check [TypeConstraint]
substituteTypeInConstraint n s (TypeConstraint m t)
  | n == m = unifyTypes s t
  | otherwise = return [TypeConstraint m (replaceTypeInType n s t)]
substituteTypeInConstraint n s (RowConstraint m r)
  = return [RowConstraint m (replaceTypeInRow n s r)]

substituteRowInConstraint :: Int -> Row -> TypeConstraint -> Check [TypeConstraint]
substituteRowInConstraint n r (TypeConstraint m t)
  = return [TypeConstraint m (replaceRowInType n r t)]
substituteRowInConstraint n r (RowConstraint m r1)
  | m == n = unifyRows r r1
  | otherwise = return [RowConstraint m (replaceRowInRow n r r1)]

replaceTypeInType :: Int -> Type -> Type -> Type
replaceTypeInType n t (TUnknown m) | m == n = t
replaceTypeInType n t (Array t1) = Array $ replaceTypeInType n t t1
replaceTypeInType n t (Object row) = Object $ replaceTypeInRow n t row
replaceTypeInType n t (Function args ret) = Function (map (replaceTypeInType n t) args) (replaceTypeInType n t ret)
replaceTypeInType n t (TypeApp t1 t2) = TypeApp (replaceTypeInType n t t1) (replaceTypeInType n t t2)
replaceTypeInType _ _ other = other

replaceTypeInRow :: Int -> Type -> Row -> Row
replaceTypeInRow n t (RCons name ty row) = RCons name (replaceTypeInType n t ty) (replaceTypeInRow n t row)
replaceTypeInRow n t other = other

replaceRowInType :: Int -> Row -> Type -> Type
replaceRowInType n r (Array t1) = Array $ replaceRowInType n r t1
replaceRowInType n r (Object row) = Object $ replaceRowInRow n r row
replaceRowInType n r (Function args ret) = Function (map (replaceRowInType n r) args) (replaceRowInType n r ret)
replaceRowInType n r (TypeApp t1 t2) = TypeApp (replaceRowInType n r t1) (replaceRowInType n r t2)
replaceRowInType _ _ other = other

replaceRowInRow :: Int -> Row -> Row -> Row
replaceRowInRow n r (RUnknown m) | m == n = r
replaceRowInRow n r (RCons name ty row) = RCons name (replaceRowInType n r ty) (replaceRowInRow n r row)
replaceRowInRow _ _ other = other

unifyTypes :: Type -> Type -> Check [TypeConstraint]
unifyTypes (TUnknown u) t = do
  guardWith "Occurs check failed" $ not $ typeOccursCheck u t
  return [TypeConstraint u t]
unifyTypes t (TUnknown u) = do
  guardWith "Occurs check failed" $ not $ typeOccursCheck u t
  return [TypeConstraint u t]
unifyTypes Number Number = return []
unifyTypes String String = return []
unifyTypes Boolean Boolean = return []
unifyTypes (Array s) (Array t) = unifyTypes s t
unifyTypes (Object row1) (Object row2) = unifyRows row1 row2
unifyTypes (Function args1 ret1) (Function args2 ret2) = do
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
  in rethrow (const $ "Cannot unify " ++ show r1 ++ " with " ++ show r2) $ do
    cs1 <- fmap concat $ mapM (uncurry unifyTypes) int
    cs2 <- unifyRows' (fromList (sd1, r1')) (fromList (sd2, r2'))
    return $ cs1 ++ cs2
  where
  unifyRows' :: Row -> Row -> Check [TypeConstraint]
  unifyRows' r (RUnknown u) = do
    guardWith "Occurs check failed" $ not $ rowOccursCheck u r
    return [RowConstraint u r]
  unifyRows' (RUnknown u) r = do
    guardWith "Occurs check failed" $ not $ rowOccursCheck u r
    return [RowConstraint u r]
  unifyRows' (RowVar v1) (RowVar v2) | v1 == v2 = return []
  unifyRows' REmpty REmpty = return []
  unifyRows' r1 r2 = throwError $ "Cannot unify " ++ show r1 ++ " with " ++ show r2 ++ "."
  toList :: Row -> ([(String, Type)], Row)
  toList (RCons name ty row) = let (tys, rest) = toList row
                               in ((name, ty):tys, rest)
  toList r = ([], r)
  fromList :: ([(String, Type)], Row) -> Row
  fromList ([], r) = r
  fromList ((name, t):ts, r) = RCons name t (fromList (ts, r))

typeOccursCheck :: Int -> Type -> Bool
typeOccursCheck u (TUnknown u') | u == u' = True
typeOccursCheck u (Array t) = typeOccursCheck u t
typeOccursCheck u (Object row) = rowOccursCheck u row
typeOccursCheck u (Function args ret) = any (typeOccursCheck u) args || typeOccursCheck u ret
typeOccursCheck u (TypeApp s t) = typeOccursCheck u s || typeOccursCheck u t
typeOccursCheck _ _ = False

rowOccursCheck :: Int -> Row -> Bool
rowOccursCheck u (RUnknown u') | u == u' = True
rowOccursCheck u (RCons name ty row) = typeOccursCheck u ty || rowOccursCheck u row
rowOccursCheck _ _ = False
