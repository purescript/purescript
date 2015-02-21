## 0.3.0 Changes

Many thanks to the [contributors](https://github.com/purescript/purescript/graphs/contributors) who helped with this release.

- Full language guide can be found [here](http://functorial.com/purescript).
- Try out the latest features [here](http://tryps.functorial.com).

## New Features / Enhancements

- Mutually recursive functions and types are now allowed within a module.
- Syntactic sugar for patterns in top-level declarations has been added. For example:

        sum [] = 0
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
- The JQuery example (`libraries/jquery/test`) has been improved to use the new `Eff` monad and `do` notation.

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

