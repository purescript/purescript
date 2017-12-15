-- |
-- The core functional representation for literal values.
--
module Language.PureScript.AST.Literals where

import Prelude.Compat
import Language.PureScript.PSString (PSString)
import Numeric.Natural (Natural)

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

-- | The various numeric literals in PureScript.
data NumericLiteral
  -- | 
  -- 32 bit signed integer literals
  -- > 32 :: Int
  = LitInt Integer
  -- | 
  -- 64 bit signed floating point literals
  -- > 32.2 :: Number
  | LitNumber Double
  -- | 
  -- 32 bit unsigned integer literals
  -- > 14u :: UInt
  | LitUInt Natural
  deriving (Show, Eq, Ord)

-- | Transform a 'NumericLiteral' using the given functions to handle the
-- various internal number representations.
foldNumericLiteral 
  :: (Integer -> r) -> (Double -> r) -> (Natural -> r) -> NumericLiteral 
  -> r
foldNumericLiteral f g k l = case l of
  LitInt n -> f n
  LitNumber n -> g n
  LitUInt n -> k n
