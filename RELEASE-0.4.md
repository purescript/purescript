## 0.4.0 Changes

This release focussed mainly on bug fixes and minor enhancements, in order to provide a stable release of the compiler.

This release saw major contributions from [a number of new contributors](https://github.com/purescript/purescript/graphs/contributors). Thank you very much to everyone who submitted pull requests, issues and suggestions, and also to the users of the #purescript FreeNode channel for their valuable feedback. As always, new contributors are always welcome!

- The full language guide can be found [here](http://purescript.readthedocs.org).
- Try out the latest features [here](http://tryps.functorial.com).

## Breaking Changes

- Multiple argument functions are no longer available.
- When used in the browser, PureScript modules are now accessed through `window.PS.<ModuleName>`. The `PS` object name can be changed via the `--browser-namespace` compiler argument.
- The `--run-main` argument has been replaced with `--main` which can now accept a module name in which to find the `main` function.

### New Features

- Dead code elimination. The new compiler argument `--module=` can be used to specify one or more modules that should be kept in the generated code along with their dependencies. If no modules are specified, all code is exported.

### Enhancements

- The `->` and `[]` types are now usable in type class instances.
- The file path is included in syntax error messages.
- Most reserved Javascript names (`return`, `const`, etc) are now usable.
- Generated code for operators is more user friendly (for example, `Prelude[">>="]`). Where this transformation cannot be applied, human-readable variable names (e.g. `$greater$greater$eq`) are generated.
- The typechecker's function application judgement was greatly simplified, and as a result, more valid things now typecheck.
- Data constructors with multiple arguments can now be defined.
- It is no longer necessary to sort modules in order of dependencies manually.
- Case statements matching data constructors no longer generate unnecessary constructor checks at runtime if there is only a single data constructor.
- New optimizer rules were added to inline JavaScript's built-in operators.

### Bug Fixes

- Floats parse correctly.
- Using data constructors in pattern matches now works properly across modules.
- PureScript modules no longer overwrite global objects with the same name.
- Show problems with shadowing of names were fixed.
- Problems with recursive non-function values were fixed.
- Type synonyms can now expand to types containing more type synonyms.
- A bug in capture-avoiding type variable substitution which was affecting the `Functor (Either e)` instance was fixed.
- Type synonym instances are now disallowed.
- Precedence rules for some basic operators were corrected.
- A bug which allowed skolem variables to escape their scope was add (thanks to Brian McKenna).
- Modules containing mutually recursive type synonyms and data declarations now compile correctly.
- A number of bugs in the optimizer were addressed.

### Libraries

- The `Prelude` has been greatly expanded. Definitions for built-in JavaScript operators have been moved into the Prelude.
- Gary Burgess has released the [grunt-purescript library](https://github.com/purescript/grunt-purescript) to build PureScript code using Grunt.
- The [purescript-parsing](https://github.com/purescript/purescript-parsing) library provides Parsec-like parsing combinators.
- The [purescript-promises](https://github.com/purescript/purescript-promises) library provides `Monad` and `Applicative` instances for promises.
- The [purescript-quickcheck](https://github.com/purescript/purescript-quickcheck) library provides a very basic implementation of a QuickCheck-style testing harness.
- The [purescript-json](https://github.com/purescript/purescript-json) library aims to provide a safe way to consume  JSON data.

### Documentation

- Documentation for the `Prelude` modules is now automatically generated and can be found [here](http://purescript.readthedocs.org/en/latest/prelude.html).
- There is now Hackage documentation.
- Code comments have been improved.
