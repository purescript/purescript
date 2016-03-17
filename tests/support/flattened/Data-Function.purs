module Data.Function
  ( flip
  , const
  , apply, ($)
  , applyFlipped, (#)
  , on
  , module Control.Category
  ) where

import Control.Category (id, compose, (<<<), (>>>))

-- | Flips the order of the arguments to a function of two arguments.
-- |
-- | ```purescript
-- | flip const 1 2 = const 2 1 = 2
-- | ```
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- | Returns its first argument and ignores its second.
-- |
-- | ```purescript
-- | const 1 "hello" = 1
-- | ```
const :: forall a b. a -> b -> a
const a _ = a

-- | Applies a function to an argument. This is primarily used as the operator
-- | `($)` which allows parentheses to be omitted in some cases, or as a
-- | natural way to apply a chain of composed functions to a value.
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

-- | Applies a function to an argument: the reverse of `(#)`.
-- |
-- | ```purescript
-- | length $ groupBy productCategory $ filter isInStock $ products
-- | ```
-- |
-- | is equivalent to:
-- |
-- | ```purescript
-- | length (groupBy productCategory (filter isInStock products))
-- | ```
-- |
-- | Or another alternative equivalent, applying chain of composed functions to
-- | a value:
-- |
-- | ```purescript
-- | length <<< groupBy productCategory <<< filter isInStock $ products
-- | ```
infixr 0 apply as $

-- | Applies an argument to a function. This is primarily used as the `(#)`
-- | operator, which allows parentheses to be ommitted in some cases, or as a
-- | natural way to apply a value to a chain of composed functions.
applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped x f = f x

-- | Applies an argument to a function: the reverse of `($)`.
-- |
-- | ```purescript
-- | products # filter isInStock # groupBy productCategory # length
-- | ```
-- |
-- | is equivalent to:
-- |
-- | ```purescript
-- | length (groupBy productCategory (filter isInStock products))
-- | ```
-- |
-- | Or another alternative equivalent, applying a value to a chain of composed
-- | functions:
-- |
-- | ```purescript
-- | products # filter isInStock >>> groupBy productCategory >>> length
-- | ```
infixl 1 applyFlipped as #

-- | The `on` function is used to change the domain of a binary operator.
-- |
-- | For example, we can create a function which compares two records based on the values of their `x` properties:
-- |
-- | ```purescript
-- | compareX :: forall r. { x :: Number | r } -> { x :: Number | r } -> Ordering
-- | compareX = compare `on` _.x
-- | ```
on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = g x `f` g y
