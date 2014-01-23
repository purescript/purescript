## Modules

Modules are introduced using the `module` keyword. All code must be placed in a module.

Introduce a module as follows, with a list of declarations:

```haskell
module A where

id x = x
```

Names may be qualified by using a dot:

```
foo = A.id
```

All the names in a module can be aliased using the `import` declaration:

```haskell
import A
```

You can also limit which names are going to be aliased:

```haskell
import A (id)
```
