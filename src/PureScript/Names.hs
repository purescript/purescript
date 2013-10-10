-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Names
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

module PureScript.Names where

import Data.Data

data Ident = Ident String | Op String deriving (Eq, Ord, Data, Typeable)

instance Show Ident where
  show (Ident s) = s
  show (Op op) = '(':op ++ ")"

