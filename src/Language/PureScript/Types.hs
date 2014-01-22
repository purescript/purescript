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
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Types where

import Data.Data
import Data.Generics (mkT, mkQ, everywhereBut)

import Language.PureScript.Names
import Language.PureScript.Unknown (Unknown(..))

data Type
  = TUnknown (Unknown Type)
  | Number
  | String
  | Boolean
  | Array
  | Object Type
  | Function [Type] Type
  | TypeVar String
  | TypeConstructor (Qualified ProperName)
  | TypeApp Type Type
  | SaturatedTypeSynonym (Qualified ProperName) [Type]
  | ForAll String Type
  | ConstrainedType [(Qualified ProperName, Type)] Type
  | Skolem Int
  | REmpty
  | RCons String Type Type deriving (Show, Eq, Data, Typeable)

rowToList :: Type -> ([(String, Type)], Type)
rowToList (RCons name ty row) = let (tys, rest) = rowToList row
                                in ((name, ty):tys, rest)
rowToList r = ([], r)

rowFromList :: ([(String, Type)], Type) -> Type
rowFromList ([], r) = r
rowFromList ((name, t):ts, r) = RCons name t (rowFromList (ts, r))

isMonoType :: Type -> Bool
isMonoType (ForAll _ _) = False
isMonoType ty = isPolyType ty

isPolyType :: Type -> Bool
isPolyType (Object ps) = all isPolyType (map snd . fst $ rowToList ps)
isPolyType (Function args ret) = all isPolyType args && isPolyType ret
isPolyType (TypeApp t1 t2) = isMonoType t1 && isMonoType t2
isPolyType (SaturatedTypeSynonym _ args) = all isPolyType args
isPolyType (ForAll _ ty) = isPolyType ty
isPolyType _ = True

mkForAll :: [String] -> Type -> Type
mkForAll = flip . foldl . flip $ ForAll

unit :: Type
unit = Object REmpty

replaceTypeVars :: (Data d) => String -> Type -> d -> d
replaceTypeVars name t = everywhereBut (mkQ False isShadowed) (mkT replaceTypeVar)
  where
  replaceTypeVar (TypeVar v) | v == name = t
  replaceTypeVar other = other
  isShadowed (ForAll v _) | v == name = True
  isShadowed _ = False
