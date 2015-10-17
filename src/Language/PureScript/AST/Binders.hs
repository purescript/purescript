-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.AST.Binders
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Case binders
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.AST.Binders where

import qualified Data.Data as D

import Language.PureScript.AST.SourcePos
import Language.PureScript.Names
import Language.PureScript.Comments
import Language.PureScript.Types

-- |
-- Data type for binders
--
data Binder
  -- |
  -- Wildcard binder
  --
  = NullBinder
  -- |
  -- A binder which matches a boolean literal
  --
  | BooleanBinder Bool
  -- |
  -- A binder which matches a string literal
  --
  | StringBinder String
  -- |
  -- A binder which matches a character literal
  --
  | CharBinder Char
  -- |
  -- A binder which matches a numeric literal
  --
  | NumberBinder (Either Integer Double)
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder (Qualified ProperName) [Binder]
  -- |
  -- A binder which matches a record and binds its properties
  --
  | ObjectBinder [(String, Binder)]
  -- |
  -- A binder which matches an array and binds its elements
  --
  | ArrayBinder [Binder]
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder Ident Binder
  -- |
  -- A binder with source position information
  --
  | PositionedBinder SourceSpan [Comment] Binder
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder Type Binder deriving (Show, Read, Eq, D.Data, D.Typeable)

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder -> [Ident]
binderNames = go []
  where
  go ns (VarBinder name) = name : ns
  go ns (ConstructorBinder _ bs) = foldl go ns bs
  go ns (ObjectBinder bs) = foldl go ns (map snd bs)
  go ns (ArrayBinder bs) = foldl go ns bs
  go ns (NamedBinder name b) = go (name : ns) b
  go ns (PositionedBinder _ _ b) = go ns b
  go ns (TypedBinder _ b) = go ns b
  go ns _ = ns
