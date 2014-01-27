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
import Data.Generics (mkT, mkQ, everywhereBut)

import Language.PureScript.Names
import Language.PureScript.Unknown (Unknown(..))

-- |
-- The type of types
--
data Type
  -- |
  -- A unification variable of type Type
  --
  = TUnknown (Unknown Type)
  -- |
  -- Javascript numbers
  --
  | Number
  -- |
  -- Javascript strings
  --
  | String
  -- |
  -- Javascript booleans
  --
  | Boolean
  -- |
  -- Javascript array type constructor
  --
  | Array
  -- |
  -- Records, parameterized by a row of types
  --
  | Object Type
  -- |
  -- A function, with zero or more arguments
  --
  | Function [Type] Type
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
  | ForAll String Type
  -- |
  -- A type with a set of type class constraints
  --
  | ConstrainedType [(Qualified ProperName, Type)] Type
  -- |
  -- A skolem constant
  --
  | Skolem Int
  -- |
  -- An empty row
  --
  | REmpty
  -- |
  -- A non-empty row
  --
  | RCons String Type Type deriving (Show, Eq, Data, Typeable)

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
isMonoType (ForAll _ _) = False
isMonoType ty = isPolyType ty

-- |
-- Check whather a type is a valid polytype
--
isPolyType :: Type -> Bool
isPolyType (Object ps) = all isPolyType (map snd . fst $ rowToList ps)
isPolyType (Function args ret) = all isPolyType args && isPolyType ret
isPolyType (TypeApp t1 t2) = isMonoType t1 && isMonoType t2
isPolyType (SaturatedTypeSynonym _ args) = all isPolyType args
isPolyType (ForAll _ ty) = isPolyType ty
isPolyType _ = True

-- |
-- Universally quantify a type
--
mkForAll :: [String] -> Type -> Type
mkForAll = flip . foldl . flip $ ForAll

-- |
-- The empty record type
--
unit :: Type
unit = Object REmpty

-- |
-- Replace a type variable, taking into account variable shadowing
--
replaceTypeVars :: (Data d) => String -> Type -> d -> d
replaceTypeVars name t = everywhereBut (mkQ False isShadowed) (mkT replaceTypeVar)
  where
  replaceTypeVar (TypeVar v) | v == name = t
  replaceTypeVar other = other
  isShadowed (ForAll v _) | v == name = True
  isShadowed _ = False
