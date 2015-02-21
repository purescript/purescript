## 0.5.0 Changes

### Breaking Changes

- Support for blocks has been removed. (paf31)
- Type class instances must now be named (paf31)

        instance showNumber :: Show Number where
          ...

- Prelude modules now follow a naming scheme similar to haskell (e.g. `Data.Maybe`, `Control.Monad`) (garyb)
- Many modules that were previously part of the Prelude have been split into individual libraries, [now distributed via Bower](http://bower.io/search/?q=purescript) (garyb)
- Multiple modules with the same name are now disallowed rather than merged (garyb)
- The `Prelude` module is now imported automatically. Conflicts can be avoided by using qualified imports or an explicit import list. (garyb, paf31)
- Overlapping instances are no longer allowed. The `Prelude` and core libraries have been updated accordingly. (paf31)
- `Functor`, `Applicative`, `Monad` are now part of a class heirarchy that include `Apply` and `Bind`. `return` is now an alias for `pure`. (joneshf, paf31, garyb)
- `Semigroupoid` is now a superclass of `Category` (garyb)
- `(:)` is now part of Prelude (garyb)
- `(!!)` has been renamed to `Prelude.Unsafe.unsafeIndex` and a safe version has been added to `Data.Array` (garyb)

### New Features

- Multi parameter typeclasses (paf31)
- Superclasses (puffnfresh, paf31)
- FlexibleInstances and FlexibleContexts (paf31)
- Let bindings are now supported. The `let` keyword can introduce several local (possibly mutually recursive) bindings, along with optional type signatures. (paf31)
- `where` clauses are now supported in value declarations, with the same rules as `let` bindings (garyb)
- Empty data declarations and empty type classes are now supported (paf31)
- A new command line option `--codegen` controls which modules will have Javascript and externs generated (paf31)
- `psc-make` now generates CommonJS-compatible modules, which can be used with `require()` in `node`. `psc` still generates modules for use in the browser. (paf31, garyb)

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
- Qualfied module imports are now supported as `import qualified M as X` (garyb)
- The escape check was removed, since it was too restrictive (paf31)
- The binary operator reordering step was greatly simplified (paf31)
- The Object type constructor can now be referenced explicitly as `Prim.Object` (with kind `# * -> *`) (paf31)
- Optimizations are now enabled by default and can be disabled with the `--no-tco` and `--no-magic-do` flags (garyb)
- Unary minus and signed numeric literals are now supported again (paf31, garyb)
- Type errors have been simplified, the full trace can be enabled with `--verbose-errors` or `-v` (paf31)
- Error messages now display source positions (paf31, garyb)
- The type classes implementation and code generation was greatly simplified (paf31)
- Object properties and row labels can now be accessed with arbitrary string names by using string literals (paf31)
- `(++)` is now an alias for the Semigroup operator `(<>)` (paf31)
- Error messages for classes with undefined or missing members have been improved (garyb)
- The SYB dependency was removed, and traversals rewritten by hand, for a large performance increase (paf31)

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
- Externs files no longer include fixity declarations for members that were removed as dead code (garyb)
- A bug which prevented `sequence $ [Just 1]` from typechecking was fixed (paf31)

### Libraries

- Purescript libraries are now [distributed via Bower](http://bower.io/search/?q=purescript). There are currently around 40 libraries available.

### Plugins

- The `grunt-purescript` plugin has been updated to provide support for new command line options.
- There is a new `gulp-purescript` plugin available for compiling with Gulp.

### Documentation

- There is a new `hierarchy` executable which will generate `.dot` diagrams based on the type class hierarchy of a module. The Prelude docs have been updated to include such a type class diagram. (joneshf)
