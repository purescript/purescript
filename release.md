## 0.3.0 Changes

Many thanks to the [contributors](https://github.com/paf31/purescript/graphs/contributors) who helped with this release.

- Full language guide can be found [here](http://functorial.com/purescript).
- Try out the latest features [here](http://tryps.functorial.com).

## New Features / Enhancements

- Mutually recursive functions and types are now allowed within a module.
- Syntactic sugar for patterns in top-level declarations has been added. For example:

        sum [] _ = []
        sum (x:xs) = x + sum xs

- Basic support for type classes has been added. Polymorphic types of the form `forall a. (C a) => ...` will not be inferred but can be checked. Type inference still works if all type class instances can be determined. There is not yet support for functionality like GHC's `FlexibleInstances`, `FlexibleContexts` or `MultiParamTypesClasses`. For example:
 
        class Truthy a where
          isTrue :: a -> Boolean
  
        instance Truthy Boolean where
          isTrue b = b
  
        instance Truthy String where
          isTrue "" = false
          isTrue _ = true
  
        ifThenElse :: forall a b. (Truthy a) => a -> b -> b -> b
        ifThenElse a tr fa = if isTrue a then tr else fa

- There is now support for `do` notation, using the `Monad` type class from the `Prelude` module. For example:

        data Maybe = Nothing | Just a
        
        instance Prelude.Monad Maybe where
          ret = Just
          (>>=) Nothing _ = Nothing
          (>>=) (Just a) f = f a
          
        test = do
          x <- Just 1
          y <- Just 2
          ret $ x + y
          
- There is now a better story for side-effects. The `Eff` module in the `Prelude` defines a monad `Eff e` where e is a row of effect types. The kind system now defines a new kind `!` of effects. Row polymorphism allows different native effects to be interleaved. For example:

        test = do
          trace "Testing"
          throwError "Error!"
          
  has inferred type `forall eff. Eff (trace :: Trace | error :: Error String | eff)`.
  See the `/examples/passing/Eff.purs` file for a more in-depth set of examples.
  Supported native effects currently include `ST`, `Trace`, `Error` and `DOM` (via the `libraries/jquery` module).
- There is a new flag `--run-main` which will check the type of the `Main.main` function and emit the call to `main();` after all other generated Javascript.
- The `Prelude` module is now automatically included, unless the `--no-prelude` flag is specified.
- The class of accepted operator names has been expanded. Specifically, `$` is now a valid operator, and is defined in the `Prelude` module.
- Tail calls can be eliminated using the `--tco` flag.
- There is basic support for checking the types of values at runtime, using the `--runtime-type-checks` flag. Right now, this only works for primitive types, and so is a work-in-progress.
- GHCJS/Fay-style foreign imports are now supported. An example is

        foreign import "function(n) { \
                      \   return function(p) {\
                      \     return Math.pow(n, p);\
                      \   }\
                      \ }" pow :: Number -> Number -> Number
- Data constructors now get imported along with their type definitions.
- The `-s` flag will now cause the compiler to use `stdin` for input.
- There is a new executable `psci` which provides simple REPL-like functionality - it parses expressions, passes the generated Javascript to Node for execution, and prints the result. This is a work-in-progress.

## Regressions

- `foreach` is no longer supported. The `Prelude` module defines `map` which should provide the same functionality.
- Support for naked expressions as statements has been removed.

## Bug Fixes

- Operator precedence rules now work across modules.
- A bug in which expressions involving non-qualified names did not typecheck was fixed.
- Trailing spaces have been removed in the generated Javascript, thanks to @utkarshkukreti.
- Type synonyms now work properly across modules.
- Module imports are not transitive, which was previously leading to some unexpected error messages.
- A bug which caused the optimizer to break generated Javascript for mutually recursive values was fixed.
- Some `Prelude` functions which called methods in the standard Javascript library have been fixed.

## 0.2.0 Changes

Many thanks to the [contributors](https://github.com/paf31/purescript/graphs/contributors) who helped with this release.

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

