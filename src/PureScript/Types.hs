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
  | TypeApp Type Type deriving Show

data Row
  = RUnknown Int
  | RowVar String
  | REmpty
  | RCons String Type Row deriving Show
