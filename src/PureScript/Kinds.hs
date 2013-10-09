-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Kinds
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

module PureScript.Kinds where

import Data.Data

data Kind
  = KUnknown Int
  | Star
  | Row
  | FunKind Kind Kind deriving (Show, Eq, Data, Typeable)
