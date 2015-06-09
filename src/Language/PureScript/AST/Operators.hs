-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.AST.Operators
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Operators fixity and associativity
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.AST.Operators where

import qualified Data.Data as D
import Data.Aeson ((.=))
import qualified Data.Aeson as A

-- |
-- A precedence level for an infix operator
--
type Precedence = Integer

-- |
-- Associativity for infix operators
--
data Associativity = Infixl | Infixr | Infix deriving (Eq, Ord, D.Data, D.Typeable)

instance Show Associativity where
  show Infixl = "infixl"
  show Infixr = "infixr"
  show Infix  = "infix"

instance A.ToJSON Associativity where
  toJSON = A.toJSON . show

-- |
-- Fixity data for infix operators
--
data Fixity = Fixity Associativity Precedence deriving (Show, Eq, Ord, D.Data, D.Typeable)

instance A.ToJSON Fixity where
  toJSON (Fixity associativity precedence) =
    A.object [ "associativity" .= associativity
             , "precedence" .= precedence
             ]
