-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Binders
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The core functional representation for binders
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.CoreFn.Binders where

import qualified Data.Data as D

import Language.PureScript.CoreFn.Literals
import Language.PureScript.Names

-- |
-- Data type for binders
--
data Binder
  -- |
  -- Wildcard binder
  --
  = NullBinder
  -- |
  -- A binder which matches a literal value
  --
  | LiteralBinder (Literal Binder)
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder (Qualified ProperName) [Binder]
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder Ident Binder deriving (Show, D.Data, D.Typeable)
