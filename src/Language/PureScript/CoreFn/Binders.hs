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
{-# LANGUAGE DeriveFunctor #-}

module Language.PureScript.CoreFn.Binders where

import qualified Data.Data as D

import Language.PureScript.CoreFn.Literals
import Language.PureScript.Names

-- |
-- Data type for binders
--
data Binder a
  -- |
  -- Wildcard binder
  --
  = NullBinder a
  -- |
  -- A binder which matches a literal value
  --
  | LiteralBinder a (Literal (Binder a))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder a Ident
  -- |
  -- A binder which matches a data constructor (type name, constructor name, binders)
  --
  | ConstructorBinder a (Qualified ProperName) (Qualified ProperName) [Binder a]
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder a Ident (Binder a) deriving (Show, Read, D.Data, D.Typeable, Functor)
