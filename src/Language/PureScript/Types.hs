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

import Control.Monad.Unify

import Language.PureScript.Names

-- |
-- The type of types
--
data Type
  -- |
  -- A unification variable of type Type
  --
  = TUnknown (TypedUnknown Type)
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
-- Type constructor for functions
--
tyFunction :: Type
tyFunction = TypeConstructor $ (Qualified $ Just $ ModuleName $ ProperName "Prelude") (ProperName "Function")

-- |
-- Type constructor for strings
--
tyString :: Type
tyString = TypeConstructor $ (Qualified $ Just $ ModuleName $ ProperName "Prelude") (ProperName "String")

-- |
-- Type constructor for numbers
--
tyNumber :: Type
tyNumber = TypeConstructor $ (Qualified $ Just $ ModuleName $ ProperName "Prelude") (ProperName "Number")

-- |
-- Type constructor for booleans
--
tyBoolean :: Type
tyBoolean = TypeConstructor $ (Qualified $ Just $ ModuleName $ ProperName "Prelude") (ProperName "Boolean")

-- |
-- Type constructor for arrays
--
tyArray :: Type
tyArray = TypeConstructor $ (Qualified $ Just $ ModuleName $ ProperName "Prelude") (ProperName "Array")

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
isMonoType (ForAll _ _) = False
isMonoType ty = True

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
