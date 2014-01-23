## Type Classes

PureScript has basic support for type classes via the `class` and `instance` keywords. 

Type class instances must be of the form `String`, `Number`, `Boolean`, `[t]` or `C t1 ... tn`, and only a single type class parameter is supported.

Type class instances are resolved based on the order in which they appeared in the source files. In particular, overlapping instances are permitted.

```haskell
class Show a where
  show :: a -> String

instance Show String where
  show s = s

instance Show Boolean where
  show true = "true"
  show false = "false"

instance (Show a) => Show [a] where
  show arr = "[" ++ showArray arr ++ "]"

showArray :: forall a. (Show a) => [a] -> String
showArray [] = ""
showArray [x] = show x
showArray (x:xs) = show x ++ ", " ++ showArray xs

example = show [true, false]
```
