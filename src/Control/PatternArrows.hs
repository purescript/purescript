-----------------------------------------------------------------------------
--
-- Module      :  Control.PatternArrows
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Arrows for Pretty Printing
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

module Control.PatternArrows where

import Prelude

import Control.Arrow ((***), (<+>))
import Control.Arrow qualified as A
import Control.Category ((>>>))
import Control.Category qualified as C
import Control.Monad.State
import Control.Monad.Fix (fix)

-- |
-- A first-order pattern match
--
-- A pattern is a Kleisli arrow for the @StateT Maybe@ monad. That is, patterns can fail, and can carry user-defined state.
--
newtype Pattern u a b = Pattern { runPattern :: A.Kleisli (StateT u Maybe) a b } deriving (A.Arrow, A.ArrowZero, A.ArrowPlus)

instance C.Category (Pattern u) where
    id = Pattern C.id
    Pattern p1 . Pattern p2 = Pattern (p1 C.. p2)

instance Functor (Pattern u a) where
  fmap f (Pattern p) = Pattern $ A.Kleisli $ fmap f . A.runKleisli p

-- |
-- Run a pattern with an input and initial user state
--
-- Returns Nothing if the pattern fails to match
--
pattern_ :: Pattern u a b -> u -> a -> Maybe b
pattern_ p u = flip evalStateT u . A.runKleisli (runPattern p)

-- |
-- Construct a pattern from a function
--
mkPattern :: (a -> Maybe b) -> Pattern u a b
mkPattern f = Pattern $ A.Kleisli (lift . f)

-- |
-- Construct a pattern from a stateful function
--
mkPattern' :: (a -> StateT u Maybe b) -> Pattern u a b
mkPattern' = Pattern . A.Kleisli

-- |
-- Construct a pattern which recursively matches on the left-hand-side
--
chainl :: Pattern u a (a, a) -> (r -> r -> r) -> Pattern u a r -> Pattern u a r
chainl g f p = fix $ \c -> g >>> ((c <+> p) *** p) >>> A.arr (uncurry f)

-- |
-- Construct a pattern which recursively matches on the right-hand side
--
chainr :: Pattern u a (a, a) -> (r -> r -> r) -> Pattern u a r -> Pattern u a r
chainr g f p = fix $ \c -> g >>> (p *** (c <+> p)) >>> A.arr (uncurry f)

-- |
-- Construct a pattern which recursively matches on one-side of a tuple
--
wrap :: Pattern u a (s, a) -> (s -> r -> r) -> Pattern u a r -> Pattern u a r
wrap g f p = fix $ \c -> g >>> (C.id *** (c <+> p)) >>> A.arr (uncurry f)

-- |
-- Construct a pattern which matches a part of a tuple
--
split :: Pattern u a (s, t) -> (s -> t -> r) -> Pattern u a r
split s f = s >>> A.arr (uncurry f)

-- |
-- A table of operators
--
data OperatorTable u a r = OperatorTable { runOperatorTable :: [ [Operator u a r] ] }

-- |
-- An operator:
--
--  [@AssocL@] A left-associative operator
--
--  [@AssocR@] A right-associative operator
--
--  [@Wrap@] A prefix-like or postfix-like operator
--
--  [@Split@] A prefix-like or postfix-like operator which does not recurse into its operand
--
data Operator u a r where
  AssocL :: Pattern u a (a, a) -> (r -> r -> r) -> Operator u a r
  AssocR :: Pattern u a (a, a) -> (r -> r -> r) -> Operator u a r
  Wrap   :: Pattern u a (s, a) -> (s -> r -> r) -> Operator u a r
  Split  :: Pattern u a (s, t) -> (s -> t -> r) -> Operator u a r

-- |
-- Build a pretty printer from an operator table and an indecomposable pattern
--
buildPrettyPrinter :: OperatorTable u a r -> Pattern u a r -> Pattern u a r
buildPrettyPrinter table p = foldl (\p' ops -> foldl1 (<+>) (flip map ops $ \case
    AssocL pat g -> chainl pat g p'
    AssocR pat g -> chainr pat g p'
    Wrap pat g -> wrap pat g p'
    Split pat g -> split pat g
  ) <+> p') p $ runOperatorTable table
