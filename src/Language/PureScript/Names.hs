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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Language.PureScript.Names where

import Data.List
import Data.Data
import Data.List.Split (splitOn)
import qualified Data.Aeson as A
import qualified Data.Text as T

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
  | Op String deriving (Eq, Ord, Data, Typeable)

runIdent :: Ident -> String
runIdent (Ident i) = i
runIdent (Op op) = op

instance Show Ident where
  show (Ident s) = s
  show (Op op) = '(':op ++ ")"

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
data Qualified a = Qualified (Maybe ModuleName) a deriving (Eq, Ord, Data, Typeable, Functor)

instance (Show a) => Show (Qualified a) where
  show (Qualified Nothing a) = show a
  show (Qualified (Just name) a) = show name ++ "." ++ show a

instance (a ~ ProperName) => A.ToJSON (Qualified a) where
  toJSON = A.toJSON . show

instance (a ~ ProperName) => A.FromJSON (Qualified a) where
  parseJSON =
    A.withText "Qualified ProperName" $ \str ->
      return $ case reverse (splitOn "." (T.unpack str)) of
        [name]      -> Qualified Nothing (ProperName name)
        (name:rest) -> Qualified (Just (reconstructModuleName rest)) (ProperName name)
        _           -> Qualified Nothing (ProperName "")
    where
    reconstructModuleName = moduleNameFromString . intercalate "." . reverse


-- |
-- Provide a default module name, if a name is unqualified
--
qualify :: ModuleName -> Qualified a -> (ModuleName, a)
qualify m (Qualified Nothing a) = (m, a)
qualify _ (Qualified (Just m) a) = (m, a)

-- |
-- Makes a qualified value from a name and module name.
--
mkQualified :: a -> ModuleName -> Qualified a
mkQualified name mn = Qualified (Just mn) name

-- |
-- Checks whether a qualified value is actually qualified with a module reference
--
isUnqualified :: Qualified a -> Bool
isUnqualified (Qualified Nothing _) = True
isUnqualified _ = False
