-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Types
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Data types for types
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Types where

import Data.Data
import Data.List (nub)

import Control.Monad.Unify
import Control.Arrow (second)
import Control.Applicative
import Control.Monad ((<=<))

import Language.PureScript.Names
import Language.PureScript.Traversals

-- |
-- An identifier for the scope of a skolem variable
--
newtype SkolemScope = SkolemScope { runSkolemScope :: Int } deriving (Show, Eq, Ord, Data, Typeable)

-- |
-- The type of types
--
data Type
  -- |
  -- A unification variable of type Type
  --
  = TUnknown Unknown
  -- |
  -- A named type variable
  --
  | TypeVar String
  -- |
  -- A type constructor
  --
  | TypeConstructor (Qualified ProperName)
  -- |
  -- A type application
  --
  | TypeApp Type Type
  -- |
  -- A type synonym which is \"saturated\", i.e. fully applied
  --
  | SaturatedTypeSynonym (Qualified ProperName) [Type]
  -- |
  -- Forall quantifier
  --
  | ForAll String Type (Maybe SkolemScope)
  -- |
  -- A type with a set of type class constraints
  --
  | ConstrainedType [(Qualified ProperName, [Type])] Type
  -- |
  -- A skolem constant
  --
  | Skolem String Int SkolemScope
  -- |
  -- An empty row
  --
  | REmpty
  -- |
  -- A non-empty row
  --
  | RCons String Type Type
  -- |
  -- A type wildcard, as would appear in a partial type synonym
  --
  | TypeWildcard
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintFunction Type Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintArray Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintObject Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintForAll [String] Type deriving (Show, Eq, Data, Typeable)

-- |
-- Convert a row to a list of pairs of labels and types
--
rowToList :: Type -> ([(String, Type)], Type)
rowToList (RCons name ty row) = let (tys, rest) = rowToList row
                                in ((name, ty):tys, rest)
rowToList r = ([], r)

-- |
-- Convert a list of labels and types to a row
--
rowFromList :: ([(String, Type)], Type) -> Type
rowFromList ([], r) = r
rowFromList ((name, t):ts, r) = RCons name t (rowFromList (ts, r))

-- |
-- Check whether a type is a monotype
--
isMonoType :: Type -> Bool
isMonoType ForAll{} = False
isMonoType _        = True

-- |
-- Universally quantify a type
--
mkForAll :: [String] -> Type -> Type
mkForAll args ty = foldl (\t arg -> ForAll arg t Nothing) ty args

-- |
-- Replace a type variable, taking into account variable shadowing
--
replaceTypeVars :: String -> Type -> Type -> Type
replaceTypeVars = replaceTypeVars' []
  where
  replaceTypeVars' bound name replacement = go bound
    where
    go :: [String] -> Type -> Type
    go _  (TypeVar v) | v == name = replacement
    go bs (TypeApp t1 t2) = TypeApp (go bs t1) (go bs t2)
    go bs (SaturatedTypeSynonym name' ts) = SaturatedTypeSynonym name' $ map (go bs) ts
    go bs f@(ForAll v t sco) | v == name = f
                             | v `elem` usedTypeVariables replacement =
                                 let v' = genName v (name : bs ++ usedTypeVariables replacement)
                                     t' = replaceTypeVars' bs v (TypeVar v') t
                                 in ForAll v' (go (v' : bs) t') sco
                             | otherwise = ForAll v (go (v : bs) t) sco
    go bs (ConstrainedType cs t) = ConstrainedType (map (second $ map (go bs)) cs) (go bs t)
    go bs (RCons name' t r) = RCons name' (go bs t) (go bs r)
    go _ ty = ty
  genName orig inUse = try 0
    where
    try :: Integer -> String
    try n | (orig ++ show n) `elem` inUse = try (n + 1)
          | otherwise = orig ++ show n

-- |
-- Replace named type variables with types
--
replaceAllTypeVars :: [(String, Type)] -> Type -> Type
replaceAllTypeVars = foldl (\f (name, ty) -> replaceTypeVars name ty . f) id

-- |
-- Collect all type variables appearing in a type
--
usedTypeVariables :: Type -> [String]
usedTypeVariables = nub . everythingOnTypes (++) go
  where
  go (TypeVar v) = [v]
  go _ = []

-- |
-- Collect all free type variables appearing in a type
--
freeTypeVariables :: Type -> [String]
freeTypeVariables = nub . go []
  where
  go :: [String] -> Type -> [String]
  go bound (TypeVar v) | v `notElem` bound = [v]
  go bound (TypeApp t1 t2) = go bound t1 ++ go bound t2
  go bound (SaturatedTypeSynonym _ ts) = concatMap (go bound) ts
  go bound (ForAll v t _) = go (v : bound) t
  go bound (ConstrainedType cs t) = concatMap (concatMap (go bound) . snd) cs ++ go bound t
  go bound (RCons _ t r) = go bound t ++ go bound r
  go _ _ = []

-- |
-- Universally quantify over all type variables appearing free in a type
--
quantify :: Type -> Type
quantify ty = foldr (\arg t -> ForAll arg t Nothing) ty $ freeTypeVariables ty

-- |
-- Move all universal quantifiers to the front of a type
--
moveQuantifiersToFront :: Type -> Type
moveQuantifiersToFront = go [] []
  where
  go qs cs (ForAll q ty sco) = go ((q, sco) : qs) cs ty
  go qs cs (ConstrainedType cs' ty) = go qs (cs ++ cs') ty
  go qs cs ty =
    let constrained = case cs of
                        [] -> ty
                        cs' -> ConstrainedType cs' ty
    in case qs of
         [] -> constrained
         qs' -> foldl (\ty' (q, sco) -> ForAll q ty' sco) constrained qs'

--
-- Traversals
--

everywhereOnTypes :: (Type -> Type) -> Type -> Type
everywhereOnTypes f = go
  where
  go (TypeApp t1 t2) = f (TypeApp (go t1) (go t2))
  go (SaturatedTypeSynonym name tys) = f (SaturatedTypeSynonym name (map go tys))
  go (ForAll arg ty sco) = f (ForAll arg (go ty) sco)
  go (ConstrainedType cs ty) = f (ConstrainedType (map (fmap (map go)) cs) (go ty))
  go (RCons name ty rest) = f (RCons name (go ty) (go rest))
  go (PrettyPrintFunction t1 t2) = f (PrettyPrintFunction (go t1) (go t2))
  go (PrettyPrintArray t) = f (PrettyPrintArray (go t))
  go (PrettyPrintObject t) = f (PrettyPrintObject (go t))
  go (PrettyPrintForAll args t) = f (PrettyPrintForAll args (go t))
  go other = f other

everywhereOnTypesTopDown :: (Type -> Type) -> Type -> Type
everywhereOnTypesTopDown f = go . f
  where
  go (TypeApp t1 t2) = TypeApp (go (f t1)) (go (f t2))
  go (SaturatedTypeSynonym name tys) = SaturatedTypeSynonym name (map (go . f) tys)
  go (ForAll arg ty sco) = ForAll arg (go (f ty)) sco
  go (ConstrainedType cs ty) = ConstrainedType (map (fmap (map (go . f))) cs) (go (f ty))
  go (RCons name ty rest) = RCons name (go (f ty)) (go (f rest))
  go (PrettyPrintFunction t1 t2) = PrettyPrintFunction (go (f t1)) (go (f t2))
  go (PrettyPrintArray t) = PrettyPrintArray (go (f t))
  go (PrettyPrintObject t) = PrettyPrintObject (go (f t))
  go (PrettyPrintForAll args t) = PrettyPrintForAll args (go (f t))
  go other = f other

everywhereOnTypesM :: (Functor m, Applicative m, Monad m) => (Type -> m Type) -> Type -> m Type
everywhereOnTypesM f = go
  where
  go (TypeApp t1 t2) = (TypeApp <$> go t1 <*> go t2) >>= f
  go (SaturatedTypeSynonym name tys) = (SaturatedTypeSynonym name <$> mapM go tys) >>= f
  go (ForAll arg ty sco) = (ForAll arg <$> go ty <*> pure sco) >>= f
  go (ConstrainedType cs ty) = (ConstrainedType <$> mapM (sndM (mapM go)) cs <*> go ty) >>= f
  go (RCons name ty rest) = (RCons name <$> go ty <*> go rest) >>= f
  go (PrettyPrintFunction t1 t2) = (PrettyPrintFunction <$> go t1 <*> go t2) >>= f
  go (PrettyPrintArray t) = (PrettyPrintArray <$> go t) >>= f
  go (PrettyPrintObject t) = (PrettyPrintObject <$> go t) >>= f
  go (PrettyPrintForAll args t) = (PrettyPrintForAll args <$> go t) >>= f
  go other = f other

everywhereOnTypesTopDownM :: (Functor m, Applicative m, Monad m) => (Type -> m Type) -> Type -> m Type
everywhereOnTypesTopDownM f = go <=< f
  where
  go (TypeApp t1 t2) = TypeApp <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (SaturatedTypeSynonym name tys) = SaturatedTypeSynonym name <$> mapM (go <=< f) tys
  go (ForAll arg ty sco) = ForAll arg <$> (f ty >>= go) <*> pure sco
  go (ConstrainedType cs ty) = ConstrainedType <$> mapM (sndM (mapM (go <=< f))) cs <*> (f ty >>= go)
  go (RCons name ty rest) = RCons name <$> (f ty >>= go) <*> (f rest >>= go)
  go (PrettyPrintFunction t1 t2) = PrettyPrintFunction <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (PrettyPrintArray t) = PrettyPrintArray <$> (f t >>= go)
  go (PrettyPrintObject t) = PrettyPrintObject <$> (f t >>= go)
  go (PrettyPrintForAll args t) = PrettyPrintForAll args <$> (f t >>= go)
  go other = f other

everythingOnTypes :: (r -> r -> r) -> (Type -> r) -> Type -> r
everythingOnTypes (<>) f = go
  where
  go t@(TypeApp t1 t2) = f t <> go t1 <> go t2
  go t@(SaturatedTypeSynonym _ tys) = foldl (<>) (f t) (map go tys)
  go t@(ForAll _ ty _) = f t <> go ty
  go t@(ConstrainedType cs ty) = foldl (<>) (f t) (map go $ concatMap snd cs) <> go ty
  go t@(RCons _ ty rest) = f t <> go ty <> go rest
  go t@(PrettyPrintFunction t1 t2) = f t <> go t1 <> go t2
  go t@(PrettyPrintArray t1) = f t <> go t1
  go t@(PrettyPrintObject t1) = f t <> go t1
  go t@(PrettyPrintForAll _ t1) = f t <> go t1
  go other = f other
