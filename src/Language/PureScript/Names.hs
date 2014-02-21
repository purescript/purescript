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
-- Data types for names
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Names where

import Data.List
import Data.Data
import Data.Function (on)

-- |
-- Names for value identifiers
--
data Ident
  -- |
  -- An alphanumeric identifier
  --
  = Ident String
  -- |
  -- A symbolic name for an infix operator
  --
  | Op String
  -- |
  -- An escaped name
  --
  | Escaped String deriving (Data, Typeable)

instance Show Ident where
  show (Ident s) = s
  show (Op op) = '(':op ++ ")"
  show (Escaped s) = s

instance Eq Ident where
  Ident s1   == Ident s2   = s1 == s2
  Op s1      == Op s2      = s1 == s2
  Escaped s1 == Escaped s2 = s1 == s2
  Ident s1   == Escaped s2 = s1 == s2
  Escaped s1 == Ident s2   = s1 == s2
  _          == _          = False

instance Ord Ident where
  compare = compare `on` show

-- |
-- Proper names, i.e. capitalized names for e.g. module names, type//data constructors.
--
newtype ProperName = ProperName { runProperName :: String } deriving (Eq, Ord, Data, Typeable)

instance Show ProperName where
  show = runProperName

-- |
-- Module names
--
data ModuleName = ModuleName [ProperName] deriving (Eq, Ord, Data, Typeable)

runModuleName :: ModuleName -> String
runModuleName (ModuleName pns) = intercalate "." (runProperName `map` pns)

moduleNameFromString :: String -> ModuleName
moduleNameFromString = ModuleName . splitProperNames
  where
  splitProperNames s = case dropWhile (== '.') s of
    "" -> []
    s' -> ProperName w : splitProperNames s''
      where (w, s'') = break (== '.') s'

instance Show ModuleName where
  show = runModuleName

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a deriving (Eq, Ord, Data, Typeable)

instance (Show a) => Show (Qualified a) where
  show (Qualified Nothing a) = show a
  show (Qualified (Just name) a) = show name ++ "." ++ show a

-- |
-- Provide a default module name, if a name is unqualified
--
qualify :: ModuleName -> Qualified a -> (ModuleName, a)
qualify m (Qualified Nothing a) = (m, a)
qualify _ (Qualified (Just m) a) = (m, a)
