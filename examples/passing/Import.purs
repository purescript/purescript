module Hello where
  id :: forall a. a -> a
  id = \x -> x

  module Nested where
    import Hello (id)

    foo = id

import Hello.Nested

main = \() -> foo 42
