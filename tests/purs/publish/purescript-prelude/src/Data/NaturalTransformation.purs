module Data.NaturalTransformation where

-- | A type for natural transformations.
-- |
-- | A natural transformation is a mapping between type constructors of kind
-- | `Type -> Type` where the mapping operation has no ability to manipulate the
-- | inner values.
-- |
-- | An example of this is the `fromFoldable` function provided in
-- | `purescript-lists`, where some foldable structure containing values of
-- | type `a` is converted into a `List a`.
-- |
-- | The definition of a natural transformation in category theory states that
-- | `f` and `g` should be functors, but the `Functor` constraint is not
-- | enforced here; that the types are of kind `Type -> Type` is enough for our
-- | purposes.
type NaturalTransformation f g = forall a. f a -> g a

infixr 4 type NaturalTransformation as ~>
