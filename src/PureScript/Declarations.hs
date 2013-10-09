-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Declarations
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

module PureScript.Declarations where

import PureScript.Values
import PureScript.Types

import Data.Data

data Declaration
  = DataDeclaration String [String] [(String, Maybe Type)]
  | TypeSynonymDeclaration String [String] Type
  | TypeDeclaration String PolyType
  | ValueDeclaration String Value
  | ExternDeclaration String PolyType deriving (Show, Data, Typeable)
