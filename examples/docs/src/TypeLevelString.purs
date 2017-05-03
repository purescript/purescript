module TypeLevelString where

data Foo

class Bar a

instance fooBar :: Fail "oops" => Bar Foo
