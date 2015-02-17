## 0.2.0 Changes

Many thanks to the [contributors](https://github.com/purescript/purescript/graphs/contributors) who helped with this release.

- Full language guide can be found [here](http://functorial.com/purescript).
- Try out the latest features [here](http://tryps.functorial.com).

### New Features

- RankNTypes

  This experimental feature allows polymorphic types (introduced with `forall`) to appear on the left of a function arrow, or as the type of a record field, etc. For example:

        rank2 :: (forall a. a -> a) -> Number
        rank2 = \f -> if f true then f 1 else f 2

- Modules
- Polymorphic Object Update

  Records now support member update in which the type of the field changes during the update. For example:

        data Wrap a = Wrap a

        update = \o -> o { prop = Wrap o.prop }

### Enhancements

- Naked expressions as statements

  Naked expressions of type `{}` can now be used as statements.

- Split code generation into `AST -> JS` and `JS -> String`

  JavaScript generation is now split into the generation of a JavaScript AST, and the pretty-printing of that AST. This enables some optimizations, and a better JavaScript pretty-printer.

- Syntactic sugar for introducing curried functions

  Single-argument functions and multiple-argument functions can now be introduced in the same way:

        test = \a (b, c) d -> a + b + c + d

- JavaScript optimizations

  Some basic dead-variable elimination, inlining and eta-conversion are now performed on the generated JavaScript.

- Generate formatted Javascript

  The generated JavaScript is now formatted to be more readable. In particular, newlines get generated, and the correct indentation is used.

- More valid infix operators

  The colon and period characters are now valid in infix operator names.

### Syntax Changes

- Avoid 'do' keyword for blocks

  `do` will be used later for Haskell-like monad comprehensions, so its previous use in introducing blocks of statements is now invalid. The `do` keyword is still reserved.

- Member foreign import syntax

  A small syntactic addition to the FFI, to support a common use case.

- Make FFI syntax match Haskell

  Use `foreign import` instead of `extern`.

- Make record declaration syntax match Haskell
- Simplify array binders

  There are now two types of array binders: cons binders `x : xs -> ...` and array binders `[x, y, z] -> ...` instead of the previously confusing `[x:xs]`.

- Array indexing syntax is ambiguous

  Array elements are now accessed using the `!!` operator: `arr !! 0` etc.

### Bug Fixes

- Allow guards access to current scope

### Libraries

- Prelude

  There is now a basic prelude in the `libraries/prelude` folder.
