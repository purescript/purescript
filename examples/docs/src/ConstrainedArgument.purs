module ConstrainedArgument where

class Foo t

type WithoutArgs      = forall a. (Partial => a) -> a
type WithArgs         = forall a. (Foo a => a) -> a
type MultiWithoutArgs = forall a. ((Partial, Partial) => a) -> a
type MultiWithArgs    = forall a b. ((Foo a, Foo b) => a) -> a

