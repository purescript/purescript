module M1 where

import Prelude ()

id :: forall a. a -> a
id = \x -> x

foo = id
