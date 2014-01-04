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

data Ident = Ident String | Op String deriving (Eq, Ord, Data, Typeable)

instance Show Ident where
  show (Ident s) = s
  show (Op op) = '(':op ++ ")"

newtype ProperName = ProperName { runProperName :: String } deriving (Eq, Ord, Data, Typeable)

instance Show ProperName where
  show = runProperName

data ModuleName = ModuleName ProperName deriving (Eq, Ord, Data, Typeable)

instance Show ModuleName where
  show (ModuleName name) = show name

data Qualified a = Qualified (Maybe ModuleName) a deriving (Eq, Ord, Data, Typeable)

instance (Show a) => Show (Qualified a) where
  show (Qualified Nothing a) = show a
  show (Qualified (Just (ModuleName name)) a) = show name ++ "." ++ show a

qualify :: ModuleName -> Qualified a -> (ModuleName, a)
qualify m (Qualified Nothing a) = (m, a)
qualify _ (Qualified (Just m) a) = (m, a)
