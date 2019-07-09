module DocCommentsDataConstructor where

data Foo
  -- | data constructor comment
  = Bar
  | Baz

data ComplexFoo a b
  = ComplexBar a
  -- | another data constructor comment
  | ComplexBaz a b

newtype NewtypeFoo
  -- | newtype data constructor comment
  = NewtypeFoo { newtypeBar :: String }
