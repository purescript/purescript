-- |
-- The core functional representation for literal values.
--
module Language.PureScript.AST.Literals 
  ( module Language.PureScript.AST.Literals 
  , NumericLiteral(..)
  , foldNumericLiteral
  ) where

import Prelude.Compat
import Language.PureScript.PSString (PSString)
import Language.PureScript.Parser.Lexer (NumericLiteral(..), foldNumericLiteral)

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A numeric literal
  --
  = NumericLiteral NumericLiteral
  -- |
  -- A string literal
  --
  | StringLiteral PSString
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
  | ObjectLiteral [(PSString, a)]
  deriving (Eq, Ord, Show, Functor)
