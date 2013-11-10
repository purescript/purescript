-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.AST
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

module Language.PureScript.CodeGen.JS.AST where

import Language.PureScript.Names
import Language.PureScript.Values

import Data.Data

data JS
  = JSNumericLiteral (Either Integer Double)
  | JSStringLiteral String
  | JSBooleanLiteral Bool
  | JSUnary UnaryOperator JS
  | JSBinary BinaryOperator JS JS
  | JSArrayLiteral [JS]
  | JSIndexer JS JS
  | JSObjectLiteral [(String, JS)]
  | JSAccessor String JS
  | JSFunction (Maybe Ident) [Ident] JS
  | JSApp JS [JS]
  | JSVar Ident
  | JSConditional JS JS JS
  | JSBlock [JS]
  | JSVariableIntroduction Ident (Maybe JS)
  | JSAssignment JSAssignment JS
  | JSWhile JS JS
  | JSFor Ident JS JS JS
  | JSIfElse JS JS (Maybe JS)
  | JSReturn JS
  | JSThrow JS deriving (Show, Data, Typeable)

data JSAssignment
  = JSAssignVariable Ident
  | JSAssignProperty String JSAssignment deriving (Show, Data, Typeable)
