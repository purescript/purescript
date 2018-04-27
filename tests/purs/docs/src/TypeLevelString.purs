module TypeLevelString where

import Prim.TypeError (class Fail, Text)

data Foo

class Bar a

instance fooBar :: Fail (Text "oops") => Bar Foo
