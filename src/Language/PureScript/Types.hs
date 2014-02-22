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
import Data.Generics (everything, mkT, mkQ, everywhereBut)

import Control.Monad.Unify
import Control.Arrow ((***))

import Language.PureScript.Names

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
  -- Javascript numbers
  --
  | Object Type
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
  | Skolem Int SkolemScope
  -- |
  -- An empty row
  --
  | REmpty
  -- |
  -- A non-empty row
  --
  | RCons String Type Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintFunction Type Type
  -- |
  -- A placeholder used in pretty printing
  --
  | PrettyPrintArray Type deriving (Show, Eq, Data, Typeable)

-- |
-- Type constructor for functions
--
tyFunction :: Type
tyFunction = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "Function")

-- |
-- Type constructor for strings
--
tyString :: Type
tyString = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "String")

-- |
-- Type constructor for numbers
--
tyNumber :: Type
tyNumber = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "Number")

-- |
-- Type constructor for booleans
--
tyBoolean :: Type
tyBoolean = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "Boolean")

-- |
-- Type constructor for arrays
--
tyArray :: Type
tyArray = TypeConstructor $ (Qualified $ Just $ ModuleName [ProperName "Prim"]) (ProperName "Array")

-- |
-- Smart constructor for function types
--
function :: Type -> Type -> Type
function t1 t2 = TypeApp (TypeApp tyFunction t1) t2

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
isMonoType (ForAll _ _ _) = False
isMonoType ty = True

-- |
-- Universally quantify a type
--
mkForAll :: [String] -> Type -> Type
mkForAll args ty = foldl (\t arg -> ForAll arg t Nothing) ty args

-- |
-- The empty record type
--
unit :: Type
unit = Object REmpty

-- |
-- Replace a type variable, taking into account variable shadowing
--
replaceTypeVars :: String -> Type -> Type -> Type
replaceTypeVars = replaceTypeVars' []
  where
  replaceTypeVars' bound name replacement = go bound
    where
    go :: [String] -> Type -> Type
    go bs (Object r) = Object $ go bs r
    go bs (TypeVar v) | v == name = replacement
    go bs (TypeApp t1 t2) = TypeApp (go bs t1) (go bs t2)
    go bs (SaturatedTypeSynonym name ts) = SaturatedTypeSynonym name $ map (go bs) ts
    go bs f@(ForAll v t sco) | v == name = f
                             | v `elem` usedTypeVariables replacement =
                                 let v' = genName v (name : bs ++ usedTypeVariables replacement)
                                     t' = replaceTypeVars' bs v (TypeVar v') t
                                 in ForAll v' (go (v' : bs) t') sco
                             | otherwise = ForAll v (go (v : bs) t) sco
    go bs (ConstrainedType cs t) = ConstrainedType (map (id *** map (go bs)) cs) (go bs t)
    go bs (RCons name t r) = RCons name (go bs t) (go bs r)
    go _ ty = ty
  genName orig inUse = try 0
    where
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
usedTypeVariables = nub . everything (++) (mkQ [] go)
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
  go bound (Object r) = go bound r
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
