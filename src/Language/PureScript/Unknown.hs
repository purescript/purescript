-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Unknown
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Data type for unification variables
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Unknown where

import Data.Data

-- |
-- The type of typed unification variables
--
newtype Unknown t = Unknown { runUnknown :: Int } deriving (Show, Eq, Ord, Data, Typeable)



