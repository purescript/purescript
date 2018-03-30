module TypeLevelString where

import Prim.TypeError (class Fail)

data Foo

class Bar a

instance fooBar :: Fail "oops" => Bar Foo
