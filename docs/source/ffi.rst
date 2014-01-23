## Foreign Function Interface

The `foreign import` keyword declares a value which is defined in Javascript, and its type:

```haskell
foreign import pow :: (Number, Number) -> Number
```

To declare a new type with no constructors, use `foreign import data` and provide the kind:

```haskell
foreign import data IO :: * -> *
	
foreign import console :: { 
  log :: String -> IO {} 
}
```
