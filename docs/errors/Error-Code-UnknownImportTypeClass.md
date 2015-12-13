This error occurs when you try to import an unknown type class. This error is often triggered by accident when trying to import a type, because PureScript has separate syntax for importing type classes and types.

The syntax for importing a type is as follows:

```purescript
import Prelude(Unit(), Ordering(..))
```

The first variant just imports the type, the second also imports all data constructors. The first variant should also be used for type synonym declarations, such as `type ListOfInt = List Int`.

The syntax for importing a type class is as follows:

```purescript
import Prelude (Semiring)
```

Note the lack of brackets.