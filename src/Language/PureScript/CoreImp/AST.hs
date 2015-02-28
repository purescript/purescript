-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Expr
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The core imperative representation
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

module Language.PureScript.CoreImp.AST where

import Data.Data

import Language.PureScript.Comments
import Language.PureScript.CoreFn.Literals
import Language.PureScript.CoreImp.Operators
import Language.PureScript.Names

-- |
-- Values and expressions
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
  -- |
  -- A data constructor (type name, constructor name, field names)
  --
  | Constructor a ProperName ProperName [Ident]
  -- |
  -- An object property accessor expression (prop, obj)
  --
  | Accessor a (Expr a) (Expr a)
  -- |
  -- An array indexer expression (index, array)
  --
  | Indexer a (Expr a) (Expr a)
  -- |
  -- An anonymous function value (arguments, body)
  --
  | AnonFunction a [Ident] [Statement a]
  -- |
  -- Function application
  --
  | App a (Expr a) [Expr a]
  -- |
  -- Variable reference
  --
  | Var a (Qualified Ident)
  -- |
  -- Partial record update
  --
  | ObjectUpdate a (Expr a) [(String, Expr a)]
  -- |
  -- Native unary operator application
  --
  | UnaryOp a UnaryOp (Expr a)
  -- |
  -- Native binary operator application
  --
  | BinaryOp a BinaryOp (Expr a) (Expr a)
  -- |
  -- Runtime value tag test, used for checking if a value was produced by a
  -- particular data constructor
  --
  | IsTagOf a (Qualified ProperName) (Expr a) deriving (Show, Data, Typeable, Functor)

-- |
-- Block/module level statements
--
data Statement a
  -- |
  -- An expression whose value is discarded (probably causes a side effect)
  --
  = Expr (Expr a)
  -- |
  -- A function introduction (name, arguments, body)
  --
  | Function a Ident [Ident] [Statement a]
  -- |
  -- A variable declaratation and initial value
  --
  | VarDecl a Ident (Expr a)
  -- |
  -- A variable assignment (assignee, value)
  --
  | Assignment a (Expr a) (Expr a)
  -- |
  -- While-style loop (condition, body)
  --
  | Loop a (Expr a) [LoopStatement a]
  -- |
  -- If-then-else statement
  --
  | IfElse a (Expr a) [Statement a] (Maybe [Statement a])
  -- |
  -- Return statement
  --
  | Return a (Expr a)
  -- |
  -- Throw statement
  --
  | Throw a String
  -- |
  -- Labelled statement
  --
  | Label a Label (Statement a)
  -- |
  -- Comment
  --
  | Comment a [Comment] deriving (Show, Data, Typeable, Functor)

-- |
-- Statement label, used for breaking out of nested loops
--
type Label = String

-- |
-- Statements possible within a loop
--
data LoopStatement a
  -- |
  -- Break statement
  --
  = Break a (Maybe Label)
  -- |
  -- Continue statement
  --
  | Continue a (Maybe Label)
  -- |
  -- Standard statement
  --
  | Statement (Statement a) deriving (Show, Data, Typeable, Functor)
