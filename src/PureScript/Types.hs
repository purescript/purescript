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

{-# LANGUAGE DeriveDataTypeable #-}

module PureScript.Types (
    Type (..),
    Row(..)
) where

import Data.Data

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
  | ForAll [String] Type deriving (Show, Data, Typeable)

data Row
  = RUnknown Int
  | RowVar String
  | REmpty
  | RCons String Type Row deriving (Show, Data, Typeable)
