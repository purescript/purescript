This error occurs when the type checker knows a token refers to a type, but is unable to find a definition for that type. For example,

```purs
x :: Maybe Number
```

yields

```
Error at  line 1, column 1 - line 1, column 6:
  Unknown type Maybe
```

if there is no accompanying definition of `Maybe`, either in the same module, or imported from another module.

A possible fix is to import the type from the relevant module:

```purs
import Data.Maybe (Maybe(..))
```

or to define it in the module:

```purs
data Maybe a = Nothing | Just a
```