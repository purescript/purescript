* Add deriving clauses to data and newtype declarations

  ```purescript
  data Color = Red | Green | Blue
    derive (Eq, Ord)

  newtype Score = Score Int
    derive newtype (Eq, Ord, Show)
    derive (Semigroup, Monoid) via (Additive Int)
  ```

  These desugar to the equivalent standalone `derive instance` declarations.
  Supports regular deriving, newtype deriving, and deriving via. Multiple
  classes per clause, multiple clauses per type.

* Add standalone `derive via` syntax

  ```purescript
  derive via (Additive Int) instance Semigroup Score
  ```

  Allows deriving an instance using a specific via type without attaching
  the clause to a data declaration.
