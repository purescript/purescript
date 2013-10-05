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

module PureScript.Kinds (
  Kind (..)
) where

data Kind
  = KUnknown Int
  | Star
  | Row
  | FunKind Kind Kind deriving (Show, Eq)
