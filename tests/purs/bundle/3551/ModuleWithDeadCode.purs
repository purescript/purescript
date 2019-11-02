module ModuleWithDeadCode (class FooBar, bar, exportThatUsesBar, foo, results) where

import Prelude

class FooBar a where
  foo :: a
  bar :: a -> Boolean

instance intFooBar :: FooBar Int where
  foo = 0
  bar _ = true

exportThatUsesBar :: forall a. (FooBar a) => a -> Boolean
exportThatUsesBar = bar

foreign import results :: { fooIsNotEliminated :: Boolean, barIsExported :: Boolean, barIsNotEliminated :: Boolean, exportThatUsesBarIsExported :: Boolean }
