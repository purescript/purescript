-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Kinds
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

module Language.PureScript.Kinds where

import Data.Data
import Language.PureScript.Unknown

data Kind
  = KUnknown (Unknown Kind)
  | Star
  | Bang
  | Row Kind
  | FunKind Kind Kind deriving (Show, Eq, Data, Typeable)
