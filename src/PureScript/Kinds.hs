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
  | FunKind Kind Kind deriving Eq

instance Show Kind where
  show (KUnknown _) = "?"
  show Star = "*"
  show Row = "R"
  show (FunKind k1 k2) = "(" ++ show k1 ++ ") -> (" ++ show k2 ++ ")"
