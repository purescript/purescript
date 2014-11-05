## 0.6 Changes

### Breaking Changes

- The `Alternative` type class hierarchy was refactored. See [here](https://github.com/purescript/purescript-control/issues/6).
- `--runtime-type-checks` has been removed. The recommended approach is to use `purescript-foreign`. (@garyb)
- The `Unit` type is now used in the Prelude and core libraries to represent values containing no data. (@garyb)
- The Prelude is no longer distributed as a separate file, but is embedded in the compiler executables. (@paf31)

### New Features

- Newtypes are now supported using the `newtype` keyword. The runtime representation of a newtype is identical to that of the contained type. (@garyb)
- Multiline string literals are now supported via triple-quote syntax, making FFI declarations much neater. (@phadej)
- Kind signatures on types and type constructor arguments are now supported. (@paf31)

### Enhancements

- The `runFnN` and `mkFnN` families of functions are now inlined by the optimizer, making interop with JavaScript functions of multiple arguments much simpler. (@paf31)
- Tail call optimization has been improved for functions using case expressions. (@paf31)
- Saturated calls to data constructors are now optimized. (@garyb)
- A new `Renamer` module now renames identifiers which shadow other names in scope, which greatly simplies code generation. (@garyb)
- `psci` now provides the following new options:
    - `:b` to browse a module (@ardumont)
    - `:s` to show current imports or  modules (@ardumont)
    - `:k` to find the kind of a type constructor (@5outh)
- The approach to checking whether a name is initialized in the generated JavaScript was simplified (@paf31)
- The dependency on the `PureScript_paths` module has been removed, which makes distribution via binaries simpler. (@paf31)
- Nested `if` blocks now get optimized. (@garyb)
- Generated code for type class dictionaries was simplified. (@garyb, @dylex)
- The code generator now inserts the version of `psc` into the file as a comment. (@co-dh)
- `()` is now valid syntax, referring to the empty row. (@paf31)
- The type checker will now display multiple errors for type errors in the same binding group. (@paf31)
- Imports can now specify hidden names using `import ... hiding ( ... )` (@andreypopp)

### Bug Fixes

- Binding group errors in type class members are now caught at compile time. (@dylex)
- Some errors related to type checking rows with duplicate labels were fixed. (@paf31)
- Some issues with the calculation of binding groups were fixed. (@paf31)
- Error messages for invalid case declarations are now generated. (@natefaubion)
- Some issues related to module exports were fixed. (@garyb)
- `psci` now checks imports for validity. (@Bogdanp)

### Libraries

- The `Alternative` type class hierarchy was refactored (@joneshf, @garyb)
- The `exceptions` library no longer supports throwing exceptions of any type.
- The following libraries have been moved to the core PureScript organisation: (@garyb)
    - `purescript-transformers`
    - `purescript-free`
    - `purescript-const`
    - `purescript-identity`
    - `purescript-lazy`
    - `purescript-distributive`
    - `purescript-bifunctors`
    - `purescript-contravariant`
    - `purescript-profunctors`
    - `purescript-maps`

### Documentation

- The [PureScript book](https://leanpub.com/purescript/read) is now available.
- The [PureScript wiki](https://github.com/purescript/purescript/wiki) is now the main resource for compiler and library documentation.
