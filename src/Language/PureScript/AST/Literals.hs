{-# LANGUAGE TemplateHaskell #-}
-- |
-- The core functional representation for literal values.
--
module Language.PureScript.AST.Literals where

import qualified Data.Aeson.TH as A

import Prelude.Compat

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | StringLiteral String
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Bool
  -- |
  -- An array literal
  --
  | ArrayLiteral [a]
  -- |
  -- An object literal
  --
  | ObjectLiteral [(String, a)]
  deriving (Eq, Ord, Show, Functor)

$(A.deriveJSON A.defaultOptions ''Literal)
