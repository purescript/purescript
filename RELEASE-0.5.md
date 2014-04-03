## 0.5.0 Changes

## Breaking Changes

- Support for blocks has been removed. (paf31)
- Type class instances must now be named (paf31)

        instance showNumber :: Show Number where
          ...

- Prelude modules now follow a naming scheme similar to haskell (e.g. `Data.Maybe`, `Control.Monad`) (garyb)
- Multiple modules with the same name are now disallowed rather than merged (garyb)
- The `--runtime-type-checks` flag has been removed. Runtime type checks have been superceded by the `purescript-foreign` library (paf31)
- The `Prelude` module is now imported automatically. Conflicts can be avoided by using qualified imports or a specific import list. (garyb, paf31)

### New Features

- Let bindings are now supported. The `let` keyword can introduce several local (possibly mutually recursive) bindings, along with optional type signatures. (paf31)
- Multi parameter typeclasses (paf31)
- Empty data declarations and empty type classes are now supported (paf31)
- A new command line option `--codegen` controls which modules will have Javascript and externs generated (paf31)
- `where` clauses are now supported in value declarations (garyb)

### Enhancements

- Pretty printing for row types was improved (garyb)
- Module names can now contain `.` (garyb)
- New optimizer rules have been added for code in the ST monad, to reproduce the functionality of the blocks feature, which has been removed (paf31)
- Pattern binders are now usable in lambda expression arguments (paf31)
- PSCI now has a `:t` command for checking the type of a value (paf31)
- Array pretty printing via `show` has been improved (joneshf)
- PSCI completions are sorted (joneshf)
- PSCI now has help commands (joneshf)
- PSCI history is in XDG config (joneshf)
- PSCI allows loading of modules from ~ paths (joneshf)
- PSCI can accept a list of modules to load on start from the command line (paf31)
- PSCI can now be configured using a `.psci` file in the current directory. If such a file exists, it should contain a list of commands to run on startup. (paf31)
- Type class instances are now named, to enable easier interop with Javascript (paf31)
- Class names no longer need to be qualified in instance declarations (garyb)
- Module exports can now be specified explicitly (garyb)
- Let bindings can now define functions with binders (paf31)
- Case statements and functions which do not pattern match on their arguments now generate smaller code (paf31)
- Imported type class instances are now exported (paf31)
- Some error messages were improved (paf31)
- Qualfied module imports are now supported as `import qualified M as X` (garyb).
- The escape check was removed, since it was too restrictive (paf31)
- The binary operator reordering step was greatly simplified (paf31)
- The Object type constructor can now be referenced explicitly as `Prim.Object` (with kind `# * -> *`) (paf31)
- Optimizations are now enabled by default and can be disabled with the `--no-tco` and `--no-magic-do` flags (garyb) 
- Unary minus and signed numeric literals are now supported again (paf31, garyb)
- Type errors have been simplified, the full trace can be enabled with `--verbose-errors` or `-v` (paf31)
- Error messages now display source positions (paf31, garyb)
 
### Bug Fixes

- The subsumes relation has been fixed for object types (paf31)
- `sort` no longer mutates arrays (joneshf)
- PSCI now evaluates expressions (joneshf)
- Overlapping variables in typeclass instances are rejected (paf31)
- A bug in the optimizer related to inlining was fixed (paf31)
- A type checker bug related to array literals was fixed (paf31)
- Externs files (`--externs`) are now working again (paf31)
- Precedence of backticked infix functions have been corrected (paf31)
- A bug which allowed some incorrect type class instances to pass the type checker was fixed (paf31)
- Type synonyms are no longer restricted to kind `*` (paf31)
- Negative number literals have been restored (garyb)
- If a type defined in a module appears in an exported declaration it must also be exported from the module (garyb)
- Error messages for unresolvable types or values include the declaration name again (garyb)
- Characters in string literals are now properly escaped (garyb)
- A module containing a single orphan type declaration and no other declarations now fails to compile (garyb)
- An error involving ordering of type class instances was fixed (garyb, paf31)

### Libraries

- Purescript libraries are now [distributed via Bower](http://bower.io/search/?q=purescript)
- The [purescript-datetime](https://github.com/purescript/purescript-datetime) library provides basic date/time functions
- The [purescript-reactive](https://github.com/purescript/purescript-reactive) and [purescript-reactive-jquery]() libraries provide reactive variables, and bindings to jQuery properties.
- The [purescript-generics](https://github.com/purescript/purescript-generics) library provides generic programming capabilities.

### Documentation

- 
