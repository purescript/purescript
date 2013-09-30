-----------------------------------------------------------------------------
--
-- Module      :  Purescript.Types
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

{-# LANGUAGE DeriveFunctor #-}

module PureScript.Types (
    Type (..),
    Row(..)
) where

import Data.List (intercalate)

data Type
  = TUnknown Int
  | Number
  | String
  | Boolean
  | Array Type
  | Object Row
  | Function [Type] Type
  | TypeVar String
  | TypeConstructor String
  | TypeApp Type Type

data Row
  = RUnknown Int
  | RowVar String
  | REmpty
  | RCons String Type Row

instance Show Type where
  show (TUnknown _) = "?"
  show Number = "Number"
  show String = "String"
  show Boolean = "Boolean"
  show (Array t) = "[" ++ show t ++ "]"
  show (Object row) = "{ " ++ show row ++ " }"
  show (Function args ret) = "(" ++ intercalate ", " (map show args) ++ ") -> " ++ show ret
  show (TypeVar v) = v
  show (TypeConstructor c) = c
  show (TypeApp t1 t2) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"

instance Show Row where
  show (RUnknown _) = "?"
  show REmpty = "{}"
  show (RowVar v) = v
  show (RCons name ty row) = name ++ " :: " ++ show ty ++ "; " ++ show row
