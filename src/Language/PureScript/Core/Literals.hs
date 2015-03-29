-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Core.Literals
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The core functional representation for literal values.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.PureScript.Core.Literals where

import qualified Data.Data as D
import qualified Data.Foldable as F
import qualified Data.Traversable as T

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
  | ObjectLiteral [(String, a)] deriving (Show, Eq, D.Data, D.Typeable, Functor, F.Foldable, T.Traversable)

-- |
-- Extracts any values from a literal.
--
extractLiteral :: Literal a -> [a]
extractLiteral (ArrayLiteral xs) = xs
extractLiteral (ObjectLiteral xs) = map snd xs
extractLiteral _ = []

-- |
-- Maps over any values within a literal.
--
modifyLiteral :: (a -> b) -> Literal a -> Literal b
modifyLiteral _ (NumericLiteral n) = NumericLiteral n
modifyLiteral _ (StringLiteral s) = StringLiteral s
modifyLiteral _ (BooleanLiteral b) = BooleanLiteral b
modifyLiteral f (ArrayLiteral ls) = ArrayLiteral (map f ls)
modifyLiteral f (ObjectLiteral ls) = ObjectLiteral (map (fmap f) ls)
