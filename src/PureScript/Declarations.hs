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
import PureScript.Names

import Data.Data

data Declaration
  = DataDeclaration String [String] [(String, Maybe Type)]
  | TypeSynonymDeclaration String [String] Type
  | TypeDeclaration Ident PolyType
  | ValueDeclaration Ident Value
  | ExternDeclaration Ident PolyType deriving (Show, Data, Typeable)
