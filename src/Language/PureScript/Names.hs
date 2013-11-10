-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Names
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

module Language.PureScript.Names where

import Data.Data
import Data.List (intercalate)

data Ident = Ident String | Op String deriving (Eq, Ord, Data, Typeable)

instance Show Ident where
  show (Ident s) = s
  show (Op op) = '(':op ++ ")"

newtype ProperName = ProperName { runProperName :: String } deriving (Eq, Ord, Data, Typeable)

instance Show ProperName where
  show = runProperName

data ModulePath = ModulePath [ProperName] deriving (Show, Eq, Ord, Data, Typeable)

subModule :: ModulePath -> ProperName -> ModulePath
subModule (ModulePath mp) name = ModulePath (mp ++ [name])

global :: ModulePath
global = ModulePath []

data Qualified a = Qualified ModulePath a deriving (Eq, Ord, Data, Typeable)

instance (Show a) => Show (Qualified a) where
  show (Qualified (ModulePath names) a) = intercalate ":" (map show names ++ [show a])

qualify :: ModulePath -> Qualified a -> (ModulePath, a)
qualify mp (Qualified (ModulePath []) a) = (mp, a)
qualify _ (Qualified mp a) = (mp, a)
