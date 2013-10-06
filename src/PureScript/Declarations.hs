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

module PureScript.Declarations (
    Declaration (..)
) where

import PureScript.Values
import PureScript.Types

import Data.Data

data Declaration
  = DataDeclaration String [String] [(String, Maybe Type)]
  | TypeSynonymDeclaration String [String] Type
  | TypeDeclaration String Type
  | ValueDeclaration String Value
  | ExternDeclaration String Type deriving (Show, Data, Typeable)
