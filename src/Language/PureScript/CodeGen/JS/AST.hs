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
-- Data types for the intermediate simplified-Javascript AST
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.CodeGen.JS.AST where

import Language.PureScript.Names
import Language.PureScript.Values

import Data.Data

-- |
-- Data type for simplified Javascript expressions
--
data JS
  -- |
  -- A numeric literal
  --
  = JSNumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | JSStringLiteral String
  -- |
  -- A boolean literal
  --
  | JSBooleanLiteral Bool
  -- |
  -- A unary operator application
  --
  | JSUnary UnaryOperator JS
  -- |
  -- A binary operator application
  --
  | JSBinary BinaryOperator JS JS
  -- |
  -- An array literal
  --
  | JSArrayLiteral [JS]
  -- |
  -- An array indexer expression
  --
  | JSIndexer JS JS
  -- |
  -- An object literal
  --
  | JSObjectLiteral [(String, JS)]
  -- |
  -- An object property accessor expression
  --
  | JSAccessor Ident JS
  -- |
  -- A function introduction (optional name, arguments, body)
  --
  | JSFunction (Maybe Ident) [Ident] JS
  -- |
  -- Function application
  --
  | JSApp JS [JS]
  -- |
  -- Variable
  --
  | JSVar Ident
  -- |
  -- Conditional expression
  --
  | JSConditional JS JS JS
  -- |
  -- A block of expressions in braces
  --
  | JSBlock [JS]
  -- |
  -- A variable introduction and optional initialization
  --
  | JSVariableIntroduction Ident (Maybe JS)
  -- |
  -- A variable assignment
  --
  | JSAssignment JSAssignment JS
  -- |
  -- While loop
  --
  | JSWhile JS JS
  -- |
  -- For loop
  --
  | JSFor Ident JS JS JS
  -- |
  -- If-then-else statement
  --
  | JSIfElse JS JS (Maybe JS)
  -- |
  -- Return statement
  --
  | JSReturn JS
  -- |
  -- Throw statement
  --
  | JSThrow JS
  -- |
  -- Type-Of operator
  --
  | JSTypeOf JS
  -- |
  -- Labelled statement
  --
  | JSLabel String JS
  -- |
  -- Break statement
  --
  | JSBreak String
  -- |
  -- Continue statement
  --
  | JSContinue String
  -- |
  -- Raw Javascript (generated when parsing fails for an inline foreign import declaration)
  --
  | JSRaw String deriving (Show, Eq, Data, Typeable)

-- |
-- Data type for expressions which can appear on the left hand side of an assignment
--
data JSAssignment
  -- |
  -- Assign a variable
  --
  = JSAssignVariable Ident
  -- |
  -- Assign an object property
  --
  | JSAssignProperty Ident JSAssignment deriving (Show, Eq, Data, Typeable)
