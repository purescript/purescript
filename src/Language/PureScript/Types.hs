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
import Language.PureScript.Names
import Language.PureScript.Unknown (Unknown(..))

data Type
  = TUnknown (Unknown Type)
  | Number
  | String
  | Boolean
  | Array Type
  | Object Row
  | Function [PolyType] Type
  | TypeVar String
  | TypeConstructor (Qualified ProperName)
  | TypeApp Type Type
  | SaturatedTypeSynonym (Qualified ProperName) [Type]
  | ForAll String Type
  | Skolem Int deriving (Show, Eq, Data, Typeable)

type PolyType = Type

data Row
  = RUnknown (Unknown Row)
  | RowVar String
  | REmpty
  | RCons String Type Row
  | RSkolem Int deriving (Show, Eq, Data, Typeable)

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

isMonoType :: Type -> Bool
isMonoType (ForAll _ _) = False
isMonoType ty = isPolyType ty

isPolyType :: Type -> Bool
isPolyType (Array ty) = isMonoType ty
isPolyType (Object ps) = all isPolyType (map snd . fst $ rowToList ps)
isPolyType (Function args ret) = all isPolyType args && isMonoType ret
isPolyType (TypeApp t1 t2) = isMonoType t1 && isMonoType t2
isPolyType (SaturatedTypeSynonym _ args) = all isMonoType args
isPolyType (ForAll idents ty) = isPolyType ty
isPolyType _ = True

mkForAll :: [String] -> Type -> Type
mkForAll = flip . foldl . flip $ ForAll
