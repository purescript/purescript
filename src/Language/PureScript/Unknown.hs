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
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Unknown where

import Data.Data

newtype Unknown t = Unknown { runUnknown :: Int } deriving (Show, Eq, Ord, Data, Typeable)



