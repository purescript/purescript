# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

* Extend ImportCompletion with declarationType (#3997, @i-am-the-slime)

  By exposing the declaration type (value, type, typeclass, etc.) 
  downstream tooling can annotate imports with this info so users know what they
  are about to import. The info can also be mapped to a namespace filter to 
  allow importing identifiers that appear more than once in a source file which
  throws an exception without such a filter. 

Bugfixes:

* Only include direct dependencies in the output for `purs graph` (#3993, @colinwahl)

Fixes a bug where the transitive closure of a module's dependencies
where included in the `depends` field in the output for `purs graph`.

* Replace PNG logo with SVG and use system color theme (#4002, @ptrfrncsmrph)

Other improvements:

* More descriptive protocol errors from the ide server (@kritzcreek)

## [v0.13.8](https://github.com/purescript/purescript/releases/tag/v0.13.8) - 2020-05-23

**Bug Fixes**

* Update incremental build cache information properly on IDE rebuilds (#3789, @kritzcreek)

  Fixes a bug where triggering a rebuild via the IDE would not update the
  `output/cache-db.json` file, which in certain situations could lead to
  unnecessary rebuilds, as well as modules not being rebuilt when they should
  have been.

* Don't include compiler-internal declarations in IDE completions (#3850, @kritzcreek)

  IDE completions would previously include pseudo-declarations such as
  `RowToList$Dict` which only exist internally, due to how type class
  desugaring inside the compiler works. These declarations are now suppressed.

* Fix corefn JSON version parsing (#3877, @paulyoung)

  Fixes a bug where the parser for the functional core (or "corefn") JSON
  format would ignore all but the first component of the compiler version
  stored in the JSON. This does not affect the compiler directly, but will be
  useful for other tooling which depends on the corefn JSON parser provided by
  the compiler library.

**Improvements**

* Add `purs graph` subcommand for graphing module dependencies (#3781, @jmackie, @f-f)

  This adds a new `graph` subcommand which allows tools to consume information
  about which modules depend on which other modules. The format is as follows:

  ```
  { "Prelude":
      { "path": "src/Prelude.purs"
      , "depends": ["Data.Semiring", "Data.Ring", ...]
      },
    "Data.Ring":
      { "path": "src/Data/Ring.purs"
      , "depends": []
      },
    ...
  }
  ```

  Each property in the returned object has exactly two properties; `path`,
  which is a string containing the file path relative to the directory where
  the command was run, and `depends`, which is an array of the names of all
  directly imported modules.

* purs ide is better at reloading changes (#3799, @kritzcreek)

  The IDE would previously sometimes miss changes that were made outside of the
  editor, like building with new dependencies or recompiling larger parts of
  the project on the console.

  The IDE will now notice when this happened on the next command issued to it
  and refresh its state before processing that command. This might cause the
  first command after an external change to take a long time to execute, but
  should increase reliability in general.

* Switch to a binary encoding for externs files (#3841, @kritzcreek)

  This change should result in significant performance improvements in both IDE
  load times and incremental builds where lots of modules are already built.

* Represent module names as a single Text value internally (#3843, @kritzcreek)

  Boosts compiler performance by representing module names as a single Text
  value, rather than a list of Text values as it was previously.

* Extract documentation for type classes in purs ide (#3856, @kritzcreek)

  This changes makes documentation comments on type classes visible to the IDE.

**Other**

* Declare explicit upper bounds on Cabal and haskeline rather than relying on
  stack's pvp-bounds (#3777, @coot)

## [v0.13.7](https://github.com/purescript/purescript/releases/tag/v0.13.7) - 2020-05-23

_release withdrawn due to CI mishap_

## [v0.13.6](https://github.com/purescript/purescript/releases/tag/v0.13.6) - 2020-01-17

**Bug Fixes**

* Reset IDE state before performing a full reload. (#3766, @kritzcreek)

  This prevents a space leak in the IDE.

* Added source spans to ado desugaring. (#3758, @dariooddenino)

  Previously errors in ado desugaring might have had no line information.

* Generate correct arity failure case for some guarded matches. (#3763, @nwolverson)

  Specifically when a multi-way case contains a pattern guard or multiple
guard expressions, the desugared case expression could contain a guard with
a different arity to the matched expressions, resulting in an error.

**Improvements**

* Improved ambiguous variable check for functional dependencies. (#3721, @MonoidMusician)

  Previously the compiler might warn about ambiguous variables that aren't actually ambiguous
due to functional dependencies. This check now fully takes functional dependencies into
consideration.

* Optimize import desugaring for full builds (#3768, @colinwahl)

  The compiler was performing redundant work when resolving dependencies for modules resulting
in poor asymptotics. This work is now shared across modules yielding a 30-40% improvement in
build times for full builds.

* Use PureScript escapes in string pretty-printing (#3751, @hdgarrood)

  Previously the compiler might print invalid escape sequences when pretty-printing code for
error messages. It now prints correctly escaped code based on PureScript's lexical grammar.

* Optimize away binds to wildcards in do-notation (#3220, @matthewleon, @hdgarrood)

  This avoids generating variable assignments if no variables are actually bound in do-notation.
Previously the compiler would emit a unique variable name that went unused.

* Output docs.json files for Prim modules (#3769, @f-f)

  This change allows downstream tools such as spago to obtain documentation data for Prim modules.
Please note, however, that the API for the docs.json files is unstable and may change without warning.

**Other**
* Fix various typos in source comments (#3760, @bwignall)

## [v0.13.5](https://github.com/purescript/purescript/releases/tag/v0.13.5) - 2019-11-13

This is a small bugfix release to address some issues which were introduced in 0.13.4.

**Bug fixes**

* Fix "too many open files" during compiling (#3743, @hdgarrood)

  The compiler would not promptly close files after opening them, which could easily lead to reaching the open file limit, causing the compiler to crash.

* Fix incorrect unused import warnings when kinds are re-exported (#3744, @hdgarrood)

  Fixes a bug in which unused import warnings were generated for kinds which were re-exported (and therefore should have been considered "used").

**Other**

* Fix Haddock markup error preventing Haddock docs being generated (#3745, @cdepillabout)
* Add upper bound on Protolude to prevent 0.2.4 from being selected (#3752, @hdgarrood)

## [v0.13.4](https://github.com/purescript/purescript/releases/tag/v0.13.4) - 2019-10-20

**Enhancements**

* Use content hashes when determining whether a file needs rebuilding (#3708, @hdgarrood)

  We now calculate and store content hashes of input files during compilation. If a file's modification time has changed since the last compile, we compare the hash to the previous hash; if the hash is unchanged, this allows us to skip rebuilding this file, speeding up the build.

* Include import declaration qualifiers in unused import warnings (#3685, @matthew-hilty)

  Previously, warnings didn't distinguish between import declarations from the same module. Code like the following
  ```purescript
  import A.B (x) -- `x` is used.
  import A.B (y) as C -- `y` is not used.
  ```
  would induce a warning like `The import of module A.B is redundant` even though only the qualified import declaration `C` is actually redundant. The warning now would be `The import of module A.B (qualified as C) is redundant`.

* Include kind imports when determining unused import warnings (#3685, @matthew-hilty)

  Previously, kind imports were ignored. The linter wouldn't emit any warnings for code like the following.
  ```purescript
  import A.B (kind K) -- `kind K` is not used.
  ```
  And the linter, disregarding `kind K`, would emit an `UnusedImport` instead of an `UnusedExplicitImport` for code like the following.
  ```purescript
  import A.B (x, kind K) -- `x` is not used, but `kind K` is.
  ```

* Better reporting of I/O errors (#3730, @hdgarrood)

  If an unexpected I/O error occurs during compiling, we now include details in the error message. For example, when trying to write compilation results onto a device which has run out of space, we previously would have received a "CannotWriteFile" error with no further information. Now, we receive the underlying error message too:

  ```
  I/O error while trying to write JSON file: ./output/cache-db.json

    ./output/cache-db.json: hClose: resource exhausted (No space left on device)
  ```

**Bug fixes**

* Improve type class resolution in the presence of constrained higher-order functions (#3558, @matthew-hilty)

  This is perhaps best illustrated with an example.
  ```purescript
  newtype LBox row a = LBox (∀ r. (∀ lbl _1. Row.Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → r) → r)

  unLBox ∷ ∀ row a r. (∀ lbl _1. Row.Cons lbl a _1 row ⇒ IsSymbol lbl ⇒ SProxy lbl → r) → LBox row a → r
  unLBox g (LBox f) = f g

  read ∷ ∀ row a. Record row → LBox row a → a
  read rec = unLBox \lbl → Record.get lbl rec
  ```

  The `read` function would previously fail with the error

  ```
  No type class instance was found for

      Prim.Row.Cons lbl4
                    a5
                    t2
                    row6
  ```

  although that dictionary should have been available in the function passed to `unLBox`. Now, it type checks successfully.

* Fix cache invalidation false negatives by storing timestamps (#3705, @hdgarrood)

  Previously, an input file would be considered 'modified', and thus requiring rebuilding on a subsequent compile, if its modification time specifies a point in time after any of the modification times of the corresponding output files.  This has turned out to be insufficient; files can often change in a way that this algorithm misses, because the input file might still have a timestamp older than the output files. Often this can happen by switching between `git` branches or by updating a dependency.

  This problem can manifest as compiler errors which don't appear to make sense or correspond to what is inside a source file, and which (until now) would need to be fixed by a clean rebuild (e.g. `rm -r output`).

  We now make a note of the modification time when we read an input file, and we consider that input file to have changed on a subsequent compile if the modification time is different to what it was before.

  The hope with this fix is that it should never be necessary to remove an output directory to get a build to run successfully. If you do run into this problem again, it is a bug: please report it.

* Fix exports incorrectly being identified as unused in purs bundle (#3727, @rhendric)

  References to properties on the `exports` object would previously not be picked up by `purs bundle` as uses of those properties, which could lead to them being incorrectly removed. For example:

  ```javascript
  'use strict';

  exports.foo = 1;
  exports.bar = exports.foo;
  ```

  would remove the `exports.foo = 1;` statement, breaking the assignment to `exports.bar`, if `foo` were not used elsewhere. This statement is now no longer removed.

* Show entire rows in type errors in the presence of the `--verbose-errors` flag (#3722, @Woody88)

  The row diffing feature, which elides common labels in rows occurring in type errors, did not previously respect the `--verbose-errors` flag, giving the same output regardless of whether it was set or not.  Now, if the flag has been supplied, we always show the entire row.

**Other**

* Add Makefile command to run license generator (#3718, @hdgarrood)
* Update language-javascript to 0.7.0.0 (@rhendric, @hdgarrood)

  This enables a number of newer JavaScript syntactic constructs to be used in FFI files. Please see the [language-javascript release notes][] for details.

* Fix for object shorthand syntax in FFI files (#3742, @hdgarrood)

[language-javascript release notes]: https://hackage.haskell.org/package/language-javascript-0.7.0.0/changelog

## [v0.13.3](https://github.com/purescript/purescript/releases/tag/v0.13.3) - 2019-08-18

**Enhancements**

* Eliminate empty type class dictionaries in generated code (#2768, @LiamGoodacre)

  Empty type class dictionaries &mdash; dictionaries which do not contain any type class member implementations at runtime &mdash; are often used to provide evidence at compile-time to justify that a particular operation will not fail; for example, `Prim.Row.Cons` can be used to justify that we can expect a record to contain a particular field with a particular type.  Unfortunately, constructing empty dictionaries can be costly, especially in more complex scenarios such as type-level programming. This release implements a new optimization which avoids the need to build empty dictionaries at runtime by instead inserting `undefined` into the generated code. This optimization can both reduce code size and improve performance in certain contexts.

* Render doc-comments for data constructors and type class members in HTML documentation (#3507, @marcosh)

  Documentation comments for data constructors and type class members are now picked up by `purs docs`, and will soon start appearing in Pursuit too. For example:

  ```purescript
  -- | Doc-comments like this one were always rendered in Pursuit
  data Maybe a =
    -- | Now this one (for the Just constructor) will be rendered too
    = Just a
    -- | And this one (for Nothing)
    | Nothing

  -- | Doc-comments like this one were always rendered in Pursuit
  class Eq a where
    -- | Now this one (for the `eq` method) will be rendered too
    eq :: a -> a -> Boolean
  ```

* Show diffs of rows in errors and hints (#3392, @dariooddenino)

  In type mismatches between rows, we now elide common labels so that the problem is easier to identify. For example, consider the following code, which has a type error due to the types of the `b` fields in the two records not matching:

  ```purescript
  foo =
    { a: 1, b: "hi", c: 3, d: 4, e: 5 }
  bar =
    { a: 1, b: 2, c: 3, d: 4, e: 5 }
  baz =
    [ foo, bar ]
  ```

  Previously, the type error would include the entirety of each record type:

  ```
  Could not match type

    String

  with type

    Int

  while trying to match type ( a :: Int
                             , b :: String
                             , c :: Int
                             , d :: Int
                             , e :: Int
                             )
  with type ( a :: Int
            , b :: Int
            , c :: Int
            , d :: Int
            , e :: Int
            )
  ```

  This can become quite difficult to read in the case of large record types. Now, we get this:

  ```
  Could not match type

    String

  with type

    Int

  while trying to match type
                             ( b :: String
                             ...
                             )

  with type
              ( b :: Int
              ...
              )
  ```

**Bug fixes**

* Remove more dead code in `purs bundle` (#3551, @rhendric)

  The dead code elimination in `purs bundle` now no longer incorrectly considers declarations to be used in the presence of local variables which happen to share their names, and is therefore able to remove these declarations when they are unused.

* Fix parsing of comma-separated guards in let statements (#3713, @natefaubion)

  The 0.13 parser would previously choke on guards separated by commas in let statements within do/ado blocks, such as

  ```purescript
  test = ado
    let
      foo
        | bar
        , baz =
          42
        | otherwise = 100
    in
      foo
  ```

  This has now been fixed.

**Other**

* Add placeholder purs.bin to fix npm installs (#3695, @hdgarrood)
* Refactor and simplify BuildPlan a little (#3699, @hdgarrood)
* Update link to partial type class guide in error message hints (#3717, @alextes)

## [v0.13.2](https://github.com/purescript/purescript/releases/tag/v0.13.2) - 2019-07-05

**Enhancements**

* Add --debug flag to `purs bundle` command (#3666, @rhendric)

  This flag causes an optimized-for-humans JSON representation of the modules
being bundled to be dumped to stderr, prior to dead code elimination.

* Ignore duplicate file inputs to CLI commands (#3653, @dyerw)

  If, after expanding globs, a particular file path appears more than once, the
compiler will now ignore the extra occurrences, instead of emitting a
`DuplicateModule` error.

**Bug fixes**

* Fix printing of tokens with string escapes (#3665, @hdgarrood)
* Fix multiple "let"s in ado before the final "in" (#3675, @natefaubion)
* Throw a parse error (not internal error) when using quoted labels as puns (#3690, @natefaubion)

**Other**

* Parser: Remove partial type signatures for parameterized productions (#3667, @natefaubion)
* Make git consider \*.out files as binary for the golden tests (#3656, @kritzcreek)
* Fix build failures on older GHCs by tightening base lower bound (#3659, @hdgarrood)
* Pin happy version to address build failures when building with Cabal (#3660, @hdgarrood)
* Add upper bounds when producing source distributions (#3661, @hdgarrood)
* Update test dependency on typelevel-prelude (#3649, @hdgarrood)
* Update author and maintainer sections of cabal file (#3663, @hdgarrood)
* Update to GHC 8.6.5, Stackage LTS 13.26 (#3688, @hdgarrood)
* Various CI maintenance (#3687, @hdgarrood)
* Move the "purescript" npm package into the compiler repo (#3691, @hdgarrood)

## [v0.13.1](https://github.com/purescript/purescript/releases/tag/v0.13.1) - 2019-07-04

_Notice: This release has been unpublished due to an error in the package tarball._

## [v0.13.0](https://github.com/purescript/purescript/releases/tag/v0.13.0) - 2019-05-30

**Grammar/Parser Changes**

`0.13` is a very exciting release for me (@natefaubion). For the past few months I've been working on a complete rewrite of the existing parser. The old parser has served us very well, but it has grown very organically over the years which means it's developed some unsightly limbs! Throughout the process I've tried to iron out a lot of dark corner cases in the language grammar, and I hope this release will set us on a firm foundation so we can start to specify what "PureScript the Language" actually is. This release is definitely breaking, but I think you'll find the changes are modest. I also hope that this release will open up a lot of opportunities for syntactic tooling, both using the existing parser or even using alternative parsers (which are now possible).

**Breaking**

There are a number of breaking changes, but I think you'll find that most code will continue to parse fine. We've tested the parser against the existing ecosystem and several large production applications at Awake, Lumi, and SlamData. The migration burden was either non-existent or only involved a few changes.

* The only whitespace now allowed in _code_ is ASCII space and line endings. Since you must use indentation to format PureScript code (unlike Haskell), we felt it was best to be more restrictive in what you can write instead of allowing potentially confusing behavior (implicit tab-width, zero-width spaces, etc). You can still use unicode whitespace within string literals.
* The only escapes accepted in string literals are `\n\r\t\'\"\\`, `\x[0-9a-fA-F]{1,6}` (unicode hex escape), and `\[\r\n ]+\` (gap escapes). We had inherited a vast zoo of escape codes from the Parsec Haskell Language parser. We decided to minimize what we support, and only add things back if there is significant demand.
* Octal and binary literals have been removed (hex remains).
* `\` is no longer a valid operator. It conflicts with lambda syntax.
* `@` is no longer a valid operator. It conflicts with named binder syntax.
* `forall` is no longer a valid identifier for expressions. We wanted a consistent rule for type identifiers and expression identifiers.
* Precedence of constructors with arguments in binders (`a@Foo b` must be `a@(Foo b)`).
* Precedence of kind annotations (`a :: Type -> Type b :: Type` must now be `(a :: Type -> Type) (b :: Type)`).
* Precedence of type annotations (`::` has lowest precedence, rather than sitting between operators and function application).
* Various edge cases with indentation/layout. Again, most code should work fine, but there were some cases where the old parser let you write code that violated the offside rule.

**Fixes**

* Many fixes around parse error locations. The new parser should yield much more precise error locations, especially for large expressions (like in HTML DSLs).
* Reported source spans no longer include whitespace and comments.
* Reported source span for the last token in a file is now correct.

**Enhancements**

* `where` is still only sugar for `let` (it does not introduce bindings over guards), but it is now usable in `case` branches in the same manner as declarations.
* `_` is now allowed in numeric literals, and is an ignored character (ie. `1_000_000 == 1000000`).
* Raw string literals (triple quotes) can now contain trailing quotes (ie. `"""hello "world"""" == "hello \"world\""`).
* Kind annotations are now allowed in `forall` contexts (#3576 @colinwahl).
* The new parser is much faster and can avoid parsing module bodies when initially sorting modules. We also do more work in parallel during the initialization phase of `purs compile`. This means that time to start compiling is faster, and incremental builds are faster. In my testing, a noop call to `purs compile` on the Awake codebase went from ~10s to ~3s.

**Other Changes**

**Breaking**

* Fix sharing in function composition inlining (#3439 @natefaubion). This is really a bugfix, but it has the potential to break code. Previously, you could write recursive point-free compositions that the compiler inadvertently eta-expanded into working code by eliminating sharing. We've changed the optimization to respect strict evaluation semantics, which can cause existing code to stack overflow. This generally arises in instance definitions. Unfortunately, we don't have a way to disallow the problematic code at this time.
* Fail compilation when a module imports itself (#3586 @hdgarrood).
* Disallow re-exporting class and type with the same name (#3648 @joneshf).

**Enhancements**

* Better illegal whitespace errors (#3627 @hdgarrood).
* Only display class members that are not exported from the module when throwing a `TransitiveExportError` for a class (#3612 @colinwahl).
* Tweaks to type pretty printing (#3610 @garyb).
* Unify matching constraints (#3620 @garyb).
* Improve error message on ModuleNotFound error for Prim modules (#3637 @ealmansi).

**Docs**

* Make markdown format behave like html. Remove --docgen opt. Separate directories for html and markdown docs (#3641 @ealmansi).
* Make html the default output format (#3643 @ealmansi).
* Write ctags and etags to filesystem instead of stdout (#3644 @ealmansi).
* Add --output option for purs docs (#3647 @hdgarrood).
* Use externs files when producing docs (#3645 @hdgarrood). `docs` is now a codegen target for `purs compile` where documentation is persisted as a `docs.json` file in the `output` directory.

**Internal**

* Remove failable patterns and `NoMonadFailDesugaring` extension (#3610 @adnelson).
* Add tests for grammar fixes addressed by CST (#3629 #3631 @hdgarrood).
* Keep Parser.y ASCII to avoid locale issues with happy (#3640 @jmackie).
* Improve display of internal errors (#3634 @hdgarrood).

## [v0.12.5](https://github.com/purescript/purescript/releases/tag/v0.12.5) - 2019-04-15

This small release fixes three issues which were introduced in 0.12.4.

**Filter out module declarations when suggesting imports (#3591)**

When determining candidates for imports, ignore modules. This allows you to easily import types which come from modules of the same name, like `Effect`. (@kRITZCREEK)

**Running purs ide server crashes on macOS (#3594)**

Running `purs ide server` on macOS would immediately crash with the error `purs: Network.Socket.listen: unsupported operation (Operation not supported on socket)`; this has now been fixed. (@f-f)

**Take qualification into consideration when determining type class cycles (#3595)**

When checking for cycles in type classes, the compiler is now able to distinguish classes which have come from different modules, meaning that e.g. `class SomeOtherModule.Foo <= Foo` is no longer incorrectly reported as a class having itself as a superclass. (@hdgarrood)

## [v0.12.4](https://github.com/purescript/purescript/releases/tag/v0.12.4) - 2019-04-07

**Enhancements**

**[purs ide] Treat module declarations like any other (#3541)**

This means we can now complete module names with the completion API as well as being able to query for module level documentation and goto-defintion for module names.

The list loadedModules command has also been deprecated, since you can now use the completion command with a filter for modules instead. (@kRITZCREEK)

**Truncate types in errors (#3401)**

Large types in error messages are now truncated. For example:

```purescript
module Main where

data Id a = Id a

foo :: Id (Id (Id (Id (Id Int))))
foo = "hi"
```

now produces

```
  Could not match type

    String

  with type

    Id (Id (Id (... ...)))
```

The previous behaviour of printing the types in full may be recovered by passing the `--verbose-errors` flag to the compiler. (@hdgarrood)

**Don't generate unused imports in JavaScript output (#2177)**

In both CommonJS compiler output and JavaScript `purs bundle` output, we no longer emit JS imports for modules whose use sites have all been optimized out. This reduces the number of warnings produced by other JavaScript bundlers or compressors such as "Side effects in initialization of unused variable Control_Category". (@rhendric)

**Simplify `purs publish` resolutions format (#3565)**

The format for resolutions files passed via the CLI to `purs publish` has been simplified. A new-style resolutions file should look something like this:

```
{
  "purescript-prelude": {
     "version": "4.0.0",
     "path": "bower_components/purescript-prelude"
  },
  "purescript-lists": {
     "version": "6.0.0",
     "path": "bower_components/purescript-lists"
  },
  ...
}
```

The `version` field is used for generating links between packages on Pursuit, and the `path` field is used to obtain the source files while generating documentation: all files matching the glob "src/**/*.purs" relative to the
`path` directory will be picked up.

The `version` field is optional, but omitting it will mean that no links will be generated for any declarations from that package on Pursuit. The "path" field is required.

The old format is still accepted, but it has been deprecated, and `purs publish` will now produce a warning when consuming it.

This change allows us to work around a bug in Bower which prevented packages with larger dependency trees (such as Halogen) from being uploaded to Pursuit (https://github.com/purescript-contrib/pulp/issues/351). (@hdgarrood)

**Improve error messages for cycles in type class declarations (#3223)**

A cycle in type class declarations, such as

```purescript
class C a <= D a
class D a <= C a
```

now produces a more informative error, which no longer confusingly refers to type synonyms, and which displays all of the classes involved in the cycle. (@Saulukass)

**Bug fixes**

* Naming a constructor `PS` no longer causes JS runtime errors when using `purs bundle` (#3505, @mhcurylo)
* `purs publish` now warns instead of failing if not all dependencies have a resolved version, e.g. if some have been installed via a branch or commit reference instead of a version range (#3061, @hdgarrood)
* Fix handling of directive prologues like "use strict" in `purs bundle` (#3581, @rhendric)

**Other**

* Raise upper bound on aeson in package.yaml (#3537, @jacereda)
* Add Nix test dependencies to stack.yaml (#3525, @jmackie)
* [purs ide] Represent filters as a data type rather than functions (#3547, @kRITZCREEK)
* Carry data constructor field names in the AST (#3566, @garyb)
* Convert prim docs tests to use tasty (#3568, @hdgarrood)
* Bump bower version used in tests (#3570, @garyb)
* Add tests for `purs bundle` (#3533, @mhcurylo)
* Update to GHC 8.6.4 (#3560, @kRITZCREEK)
* Rerun some of the compiler tests to test with `purs bundle` (#3579, @rhendric)

## [v0.12.3](https://github.com/purescript/purescript/releases/tag/v0.12.3) - 2019-02-24

**Enhancements**

- Add better positions for UnknownName errors for types/kinds (#3515, @colinwahl)

    Previously an UnknownName error (arising from e.g. referring to a non-existent type, or a type which you forgot to import) would have a span covering the whole type annotation. Now, the error span only covers the relevant part of the type.

- Boost performance of `purs docs` by simplifying re-export handling (#3534, @hdgarrood)

**Bug fixes**

- Fix applicative do notation breaking API documentation generation with `purs docs` (#3414, @hdgarrood)
- Fix the REPL browser backend (#3387, @dariooddenino)

**Other**

- Make the license generator a proper stack script (@kRITZCREEK)
- Include the module from which something was imported for re-exports in externs files (@hdgarrood)
- Add AppVeyor build status to README.md (@hdgarrood)

## [v0.12.2](https://github.com/purescript/purescript/releases/tag/v0.12.2) - 2019-01-13

**New features**

- Named type wildcards (#3500, @natefaubion)

  It's now possible to use `?hole` style syntax in type signatures where you want the compiler to tell you the missing type. This was previously possible by using `_` in a type signature, but now `_` can be used without raising a warning, as long as it does not appear in a top level declaration.

**Enhancements**

- Improve error message for missing node.js in the repl (#3456, @justinwoo)
- Add `Boolean` kind to `Prim.Boolean` (#3389, @justinwoo)
- Link to documentation repo as docs for non-Prim built-in types/kinds (#3460, @JordanMartinez)
- PSCi: Support multiple command types in paste-mode (#3471, @LiamGoodacre)
- Add `row:column` printing for source positions in error messages (#3473, @justinwoo)
- Add `:print` directive for customizable repl printing (#3478, @hdgarrood)
- Implement qualified `do` (#3373, @pkamenarsky)
- Add better source positions to kind errors (#3495, @natefaubion)

**Fixes**

- Remove references to previous kinds `*` and `!` (#3458, @LiamGoodacre)
- Fix linting of unused type variables (#3464, @LiamGoodacre)
- Avoid dropping super class dicts for the same class (#3461, @LiamGoodacre)
- Fix issue where `Partial` can foil TCO optimizations (#3218, @matthewleon)
- Fix quoting of record labels in error messages (#3480, @hdgarrood)
- Prevent invalid JS being generated from awkward record labels (#3486, @hdgarrood)
- Fix unnecessary quoting of reserved names when used as labels (#3487, @hdgarrood)
- Fix source spans for binding groups (#3462, @LiamGoodacre)
- Fix kind error for recursive data type (#3511, @natefaubion)

**Other (internals)**

- Add annotations to `Type` and `Kind` (#3484, @natefaubion)
- Use handwritten JSON instances for `Type`/`Kind` (#3496, @natefaubion)
- Remove pretty print constructors from `Type` (#3498, @natefaubion)
- Add JSON compatibility tests (#3497, @hdgarrood)
- Remove the concept of the 'current module' in Docs (#3506, @hdgarrood)

## [v0.12.1](https://github.com/purescript/purescript/releases/tag/v0.12.1) - 2018-11-12

**Enhancements**

* Print types of missing typeclass members (#3398, @fehrenbach)
* Added `Prim.TypeError.QuoteLabel` for pretty printing labels in custom type errors (#3436, @dariooddenino)
* `purs ide` accepts codegen targets for the rebuild command (#3449, @kRITZCREEK)

**Fixes**

* Fixes errors spans for `CannotFindDerivingType` (#3425, @kRITZCREEK)
* Fixes a traversal bug where `ObjectNestedUpdate` was surviving desugaring (#3388, @natefaubion)
* Fixes type operators reexports (#3410, @natefaubion)
* Fixes ST magic-do and inlining (#3444, @natefaubion)
* Fixes missing span information when using do-syntax without importing `bind` or `discard` (#3418, @natefaubion)
* Fixes missing span information when shadowing an open import with a module definition (#3417, @natefaubion)
* Fixes stale `:browse` environment after `:reload` (#3001, @rndnoise)

**Other**

* Fix test-support dependency versions and update psci browse test (#3374, @LiamGoodacre)
* Changes to build with GHC 8.4.3 (#3372, @kRITZCREEK)
* Set --haddock flag based on BUILD_TYPE (#3409, @justinwoo)
* Use `microlens-platform` instead of `lens` (#3400, @joneshf)
* Avoid `Data.ByteString.Lazy.toStrict` (#3433, @coot)
* Add ffiCodegen to MakeActions (#3434, @coot)
* Add nix config to stack.yaml (#3435, @f-f)

## [v0.12.0](https://github.com/purescript/purescript/releases/tag/v0.12.0) - 2018-05-21

**Breaking changes**

- Added applicative-do notation; `ado` is now a keyword. An full explanation of the behaviour and usage of `ado` is available [in a comment on the issue](https://github.com/purescript/purescript/pull/2889#issuecomment-301260299). (#2889, @rightfold)
- Removed wrapper scripts for the old binary names (psc, psci, etc.) (#2993, @hdgarrood)
- Removed compiler support for deriving `purescript-generics`. `purescript-generics-rep` is still supported. (#3007, @paf31)
- Instances with just one method now require the method to be indented (bug fix, but potentially breaking) (#2947, @quesebifurcan)
- Overlapping instances are now an error rather than a warning, but can be resolved with the new instance chain groups feature (#2315, @LiamGoodacre)
- Reworked the `CoreFn` json representation. This change enables use of the [Zephyr tree shaking tool](https://github.com/coot/zephyr) for PureScript. (#3049, #3342, @coot)
- It is no longer possible to export a type class that has superclasses that are not also exported (bug fix, but potentially breaking) (#3132, @parsonsmatt)
- `Eq` and `Ord` deriving will now rely on `Eq1` and `Ord1` constraints as necessary where sometimes previously `Eq (f _)` would be required. `Eq1` and `Ord1` instances can also be derived. (#3207, @garyb)
- Some `Prim` type classes have been renamed/moved, so will require explicit importing (#3176, @parsonsmatt):
    - `RowCons` is now `Prim.Row.Cons`
    - `Union` is now `Prim.Row.Union`
    - `Fail` is now `Prim.TypeError.Fail`
    - `Warn` is now `Prim.TypeError.Warn`
- Users can no longer specify modules under the `Prim` namespace (#3291, @parsonsmatt)
- `TypeConcat` and `TypeString` have been replaced because they were in kind `Symbol` but weren't literals.  The `Prim.TypeError.Doc` kind and related constructors (`Text`, `Quote`, `Beside`, `Above`) have been added in their place.  The `Fail` and `Warn` type classes now accept a `Doc` instead of a `Symbol`.
 (#3134, @LiamGoodacre)
- In simple cases instance overlaps are now checked at declaration time rather than being deferred until an attempt is made to use them. (#3129, @LiamGoodacre)
- Chaining non-associative or mixed associativity operators of the same precedence is no longer allowed (#3315, @garyb)
- The `--dump-corefn` and `--source-maps` arguments to `purs compile` have been removed. There is now a `--codegen` argument that allows the specific codegen targets to be specified - for example, `--codegen corefn` will not produce JS files, `--codgen js,corefn` will produce both. If the `sourcemaps` target is used `js` will be implied, so there's no difference between `--codegen js,sourcemaps` and `--codegen sourcemaps`). If no targets are specified the default is `js`. (#3196, @garyb, @gabejohnson)
- Exported types that use foreign kinds now require the foreign kinds to be exported too (bug fix, but potentially breaking) (#3331, @garyb)
- The pursuit commands were removed from `purs ide` due to lack of use and editor tooling implementing the features instead (#3355, @kRITZCREEK)

**Enhancements**

- Added `Cons` compiler-solved type class for `Symbol` (#3054, @kcsongor)
- The `Append` compiler-solved type class for `Symbol` can now be run in reverse (#3025, @paf31)
- Find Usages for values and constructors in `purs ide` (#3206, @kRITZCREEK)
- `purs ide` treats `hiding` imports the same as open imports when sorting (#3069, @kRITZCREEK)
- Added inlining for fully saturated usages of `runEffFn/mkEffFn` (#3026, @nwolverson)
- Improved explanation of `UnusableDeclaration` error (#3088, #3304, @i-am-tom)
- Improved rendering of comments in generated JavaScript by removing additional newlines (#3096, @brandonhamilton)
- Instance chain support. (#2315, @LiamGoodacre)
    > We can now express an explicit ordering on instances that would previously have been overlapping.
    > For example we could now write an `IsEqual` type class to compute if two types are equal or apart:
    > ```
    > class IsEqual (l :: Type) (r :: Type) (o :: Boolean) | l r -> o
    > instance isEqualRefl :: IsEqual x x True
    > else instance isEqualContra :: IsEqual l r False
    > ```
    > Note the `else` keyword that links the two instances together.
    > The `isEqualContra` will only be up for selection once the compiler knows it couldn't possible select `isEqualRefl` - i.e that `l` and `r` are definitely not equal.
- Improved orphan instance error to include locations where the instance would be valid (#3106, @i-am-tom)
- Added an explicit error for better explanation of duplicate type class or instance declarations (#3093, @LiamGoodacre)
- `purs ide` now provide documentation comments (#2349, @nwolverson)
- Clarified meaning of duplicate labels in a `Record` row (#3143, @paf31)
- Explicit import suggestions consistently use `(..)` for constructors now (#3142, @nwolverson)
- Improved tab completion in `purs repl` (#3227, @rndnoise)
- Large compiler perfomance improvement in some cases by skipping source spans in `Eq`, `Ord` for binders (#3265, @bitemyapp)
- Added support for error/warning messages to carry multiple source spans (#3255, @garyb)
- Improved tab completion in `purs repl` when parens and brackets are involved (#3236, @rndnoise)
- Improved completion in `purs repl` after `:kind` and `:type` (#3237, @rndnoise)
- Added the "magic do" optimisation for the new simplified `Effect` type (`Control.Monad.Eff` is still supported) (#3289, @kRITZCREEK, #3301, @garyb)
- Improvide build startup times when resuming a build with incremental results (#3270, @kRITZCREEK)
- Added compiler-solved `Prim.Row.Nub` type class (#3293, @natefaubion)
- Improved docs for `Prim.Row.Cons` and `Prim.Row.Union` (#3292, @vladciobanu)
- `Functor` can now be derived when quantifiers are used in constructors (#3232, @i-am-tom)
- `purs repl` will now complete types after `::` (#3239, @rndnoise)
- Added compiler-solved `Prim.Row.Lacks` type class (#3305, @natefaubion)
- Added current output path to missing output error message from `purs ide` (#3311, @rgrinberg)
- Improved parser error messages for `.purs-repl` (#3248, @rndnoise)
- `require` in generated JavaScript now includes full `index.js` file paths (#2621, @chexxor)
- Added more compiler-solved type classes and supporting types and kinds to `Prim`:
    - `Prim.Ordering` module with `kind Ordering`, `type LT`, `type EQ`, `type GT`
    - `Prim.RowList` module with `class RowToList`, `kind RowList`, `type Nil`, `type Cons`
    - `Prim.Symbol` module with `class Compare`, `class Append`, `class Cons`
    (#3312, @LiamGoodacre, @kRITZCREEK)
- Generated code for closed records now explicitly reconstructs the record rather than looping (#1493, @fehrenbach, [blog post with more details](http://stefan-fehrenbach.net/blog/2018-04-28-efficient-updates-closed-records-purescript/index.html))
- Enhanced `purs --help` message to include hint about using `--help` with commands (#3344, @hdgarrood)
- `IncorrectConstructorArity` error message now includes a hint of how many arguments are expected for the constructor (#3353, @joneshf)
- `purs ide` now uses absolute locations for file paths for better experience in some editors (#3363, @kRITZCREEK)

**Bug fixes**

- Fixed a bug with names cause by `Prim` always being imported unqualified (#2197, @LightAndLight)
- Fixed overlapping instances error message to reflect its new status as an error (#3084, @drets)
- Added source position to `TypeClassDeclaration` errors (#3109, @b123400)
- Fixed entailment issues with skolems and matches in the typechecker (#3121, @LiamGoodacre)
- Fixed multiple parentheses around a type causing a crash (#3085, @MonoidMusician)
- Fixed `purs ide` inserting conflicting imports for types (#3131, @nwolverson)
- Fixed constraints being inferred differently for lambda expressions compared with equational declarations (#3125, @LiamGoodacre)
- Updated glob handling to prevent excessive memory usage (#3055, @hdgarrood)
- Added position information to warnings in type declarations (#3174, @b123400)
- Fixed documentation generated for Pursuit rendering functional dependency variables as identifier links (#3180, @houli)
- Naming a function argument `__unused` no longer breaks codegen (#3187, @matthewleon)
- Added position information to `ShadowedName` warning (#3213, @garyb)
- Added position information to `UnusedTypeVar` warning (#3214, @garyb)
- Added position information to `MissingClassMember`, `ExtraneousClassMember`, `ExpectedWildcard` errors (#3216, @garyb)
- Added position information to `ExportConflict` errors (#3217, @garyb)
- Fixed `ctags` and `etags` generation when explicit exports are involved (#3204, @matthewleon)
- Added position information to `ScopeShadowing` warning (#3219, @garyb)
- Added position information for various FFI related errors and warnings (#3276, @garyb)
- Added all available positions to `CycleInModule` and `DuplicateModule` errors (#3273, @garyb)
- Added position information for `IntOutOfRange` errors (#3277, @garyb, @kRITZCREEK)
- Warnings are now raised when a module re-exports a qualified module with implicit import (#2726, @garyb)
- `purs repl` now shows results for `:browse Prim` (#2672, @rndnoise)
- Added position information to `ErrorParsingFFIModule` (#3307, @nwolverson)
- Added position information for `ScopeConflict` cause by exports (#3318, @garyb)
- Added position information to errors that occur in binding groups and data binding groups (#3275, @garyb)
- Fixed a scoping issue when resolving operators (#2803, @kRITZCREEK, @LightAndLight)
- Type synonyms are now desugared earlier when newtype deriving (#3325, @LiamGoodacre)
- Fixed subgoals of compiler-solved type classes being ignored (#3333, @LiamGoodacre)
- Added position information to type operator associativity errors (#3337, @garyb)
- Updated description of `purs docs` command (#3343, @hdgarrood)
- Fixed `purs docs` issue with re-exporting from `Prim` submodules (#3347, @hdgarrood)
- Enabled `purs ide` imports for `Prim` submodules (#3352, @kRITZCREEK)
- Fixed `purs bundle` failing to bundle in the 0.12-rc1 (#3359, @garyb)
- Enabled `:browse` for `Prim` submodules in `purs repl` (#3364, @kRITZCREEK)

**Other**

- Updated installation information to include details about prebuild binaries (#3167, @MiracleBlue)
- Test suite now prints output when failing cases are encountered (#3181, @parsonsmatt)
- Updated test suite to use tasty (#2848, @kRITZCREEK)
- Improved performance of `repl` test suite (#3234, @rndnoise)
- Refactored `let` pattern desugaring to be less brittle (#3268, @kRITZCREEK)
- Added makefile with common tasks for contributors (#3266, @bitemyapp)
- Added `ghcid` and testing commands to makefile (#3290, @parsonsmatt)
- Removed old unused `MultipleFFIModules` error (#3308, @nwolverson)
- `mod` and `div` for `Int` are no longer inlined as their definition has changed in a way that makes their implementation more complicated - purescript/purescript-prelude#161 (#3309, @garyb)
- The test suite now checks warnings and errors have position information (#3211, @garyb)
- The AST was updated to be able to differentiate between `let` and `where` clauses (#3317, @joneshf)
- Support for an optimization pass on `CoreFn` was added (#3319, @matthewleon)
- Clarified note in the `purs ide` docs about the behaviour of `--editor-mode` (#3350, @chexxor)
- Updated bundle/install docs for 0.12 (#3357, @hdgarrood)
- Removed old readme for `psc-bundle` (a leftover from before the unified `purs` binary) (#3356, @Cmdv)

## [v0.12.0-rc1](https://github.com/purescript/purescript/releases/tag/v0.12.0-rc1) - 2018-04-29

**Breaking changes**

- Added applicative-do notation; `ado` is now a keyword. An full explanation of the behaviour and usage of `ado` is available [in a comment on the issue](https://github.com/purescript/purescript/pull/2889#issuecomment-301260299). (#2889, @rightfold)
- Removed wrapper scripts for the old binary names (psc, psci, etc.) (#2993, @hdgarrood)
- Removed compiler support for deriving `purescript-generics`. `purescript-generics-rep` is still supported. (#3007, @paf31)
- Instances with just one method now require the method to be indented (bug fix, but potentially breaking) (#2947, @quesebifurcan)
- Overlapping instances are now an error rather than a warning, but can be resolved with the new instance chain groups feature (#2315, @LiamGoodacre)
- Reworked the `CoreFn` json representation (#3049, @coot)
- It is no longer possible to export a type class that has superclasses that are not also exported (bug fix, but potentially breaking) (#3132, @parsonsmatt)
- `Eq` and `Ord` deriving will now rely on `Eq1` and `Ord1` constraints as necessary where sometimes previously `Eq (f _)` would be required. `Eq1` and `Ord1` instances can also be derived. (#3207, @garyb)
- Some `Prim` type classes have been renamed/moved, so will require explicit importing (#3176, @parsonsmatt):
    - `RowCons` is now `Prim.Row.Cons`
    - `Union` is now `Prim.Row.Union`
    - `Fail` is now `Prim.TypeError.Fail`
    - `Warn` is now `Prim.TypeError.Warn`
- Users can no longer specify modules under the `Prim` namespace (#3291, @parsonsmatt)
- `TypeConcat` and `TypeString` have been replaced because they were in kind `Symbol` but weren't literals.  The `Prim.TypeErrer.Doc` kind and related constructors (`Text`, `Quote`, `Beside`, `Above`) have been added in their place.  The `Fail` and `Warn` type classes now accept a `Doc` instead of a `Symbol`.
 (#3134, @LiamGoodacre)
- In simple cases instance overlaps are now checked at declaration time rather than being deferred until an attempt is made to use them. (#3129, @LiamGoodacre)
- Chaining non-associative or mixed associativity operators of the same precedence is no longer allowed (#3315, @garyb)
- The `--dump-corefn` and `--source-maps` arguments to `purs compile` have been removed. There is now a `--codegen` argument that allows the specific codegen targets to be specified - for example, `--codegen corefn` will not produce JS files, `--codgen js,corefn` will produce both. If the `sourcemaps` target is used `js` will be implied, so there's no difference between `--codegen js,sourcemaps` and `--codegen sourcemaps`). If no targets are specified the default is `js`. (#3196, @garyb, @gabejohnson)
- Exported types that use foreign kinds now require the foreign kinds to be exported too (bug fix, but potentially breaking) (#3331, @garyb)

**Enhancements**

- Added `Cons` compiler-solved type class for `Symbol` (#3054, @kcsongor)
- The `Append` compiler-solved type class for `Symbol` can now be run in reverse (#3025, @paf31)
- Find Usages for values and constructors in `purs ide` (#3206, @kRITZCREEK)
- `purs ide` treats `hiding` imports the same as open imports when sorting (#3069, @kRITZCREEK)
- Added inlining for fully saturated usages of `runEffFn/mkEffFn` (#3026, @nwolverson)
- Improved explanation of `UnusableDeclaration` error (#3088, #3304, @i-am-tom)
- Improved rendering of comments in generated JavaScript by removing additional newlines (#3096, @brandonhamilton)
- Instance chain support. (#2315, @LiamGoodacre)
    > We can now express an explicit ordering on instances that would previously have been overlapping.
    > For example we could now write an `IsEqual` type class to compute if two types are equal or apart:
    > ```
    > class IsEqual (l :: Type) (r :: Type) (o :: Boolean) | l r -> o
    > instance isEqualRefl :: IsEqual x x True
    > else instance isEqualContra :: IsEqual l r False
    > ```
    > Note the `else` keyword that links the two instances together.
    > The `isEqualContra` will only be up for selection once the compiler knows it couldn't possible select `isEqualRefl` - i.e that `l` and `r` are definitely not equal.
- Improved orphan instance error to include locations where the instance would be valid (#3106, @i-am-tom)
- Added an explicit error for better explanation of duplicate type class or instance declarations (#3093, @LiamGoodacre)
- `purs ide` now provide documentation comments (#2349, @nwolverson)
- Clarified meaning of duplicate labels in a `Record` row (#3143, @paf31)
- Explicit import suggestions consistently use `(..)` for constructors now (#3142, @nwolverson)
- Improved tab completion in `purs repl` (#3227, @rndnoise)
- Large compiler perfomance improvement in some cases by skipping source spans in `Eq`, `Ord` for binders (#3265, @bitemyapp)
- Added support for error/warning messages to carry multiple source spans (#3255, @garyb)
- Improved tab completion in `purs repl` when parens and brackets are involved (#3236, @rndnoise)
- Improved completion in `purs repl` after `:kind` and `:type` (#3237, @rndnoise)
- Added the "magic do" optimisation for the new simplified `Effect` type (`Control.Monad.Eff` is still supported) (#3289, @kRITZCREEK, #3301, @garyb)
- Improvide build startup times when resuming a build with incremental results (#3270, @kRITZCREEK)
- Added compiler-solved `Prim.Row.Nub` type class (#3293, @natefaubion)
- Improved docs for `Prim.Row.Cons` and `Prim.Row.Union` (#3292, @vladciobanu)
- `Functor` can now be derived when quantifiers are used in constructors (#3232, @i-am-tom)
- `purs repl` will now complete types after `::` (#3239, @rndnoise)
- Added compiler-solved `Prim.Row.Lacks` type class (#3305, @natefaubion)
- Added current output path to missing output error message from `purs ide` (#3311, @rgrinberg)
- Improved parser error messages for `.purs-repl` (#3248, @rndnoise)
- `require` in generated JavaScript now includes full `index.js` file paths (#2621, @chexxor)
- Added more compiler-solved type classes and supporting types and kinds to `Prim`:
    - `Prim.Ordering` module with `kind Ordering`, `type LT`, `type EQ`, `type GT`
    - `Prim.RowList` module with `class RowToList`, `kind RowList`, `type Nil`, `type Cons`
    - `Prim.Symbol` module with `class Compare`, `class Append`, `class Cons`
    (#3312, @LiamGoodacre, @kRITZCREEK)
- Generated code for closed records now explicitly reconstructs the record rather than looping (#1493, @fehrenbach, [blog post with more details](http://stefan-fehrenbach.net/blog/2018-04-28-efficient-updates-closed-records-purescript/index.html))

**Bug fixes**

- Fixed a bug with names cause by `Prim` always being imported unqualified (#2197, @LightAndLight)
- Fixed overlapping instances error message to reflect its new status as an error (#3084, @drets)
- Added source position to `TypeClassDeclaration` errors (#3109, @b123400)
- Fixed entailment issues with skolems and matches in the typechecker (#3121, @LiamGoodacre)
- Fixed multiple parentheses around a type causing a crash (#3085, @MonoidMusician)
- Fixed `purs ide` inserting conflicting imports for types (#3131, @nwolverson)
- Fixed constraints being inferred differently for lambda expressions compared with equational declarations (#3125, @LiamGoodacre)
- Updated glob handling to prevent excessive memory usage (#3055, @hdgarrood)
- Added position information to warnings in type declarations (#3174, @b123400)
- Fixed documentation generated for Pursuit rendering functional dependency variables as identifier links (#3180, @houli)
- Naming a function argument `__unused` no longer breaks codegen (#3187, @matthewleon)
- Added position information to `ShadowedName` warning (#3213, @garyb)
- Added position information to `UnusedTypeVar` warning (#3214, @garyb)
- Added position information to `MissingClassMember`, `ExtraneousClassMember`, `ExpectedWildcard` errors (#3216, @garyb)
- Added position information to `ExportConflict` errors (#3217, @garyb)
- Fixed `ctags` and `etags` generation when explicit exports are involved (#3204, @matthewleon)
- Added position information to `ScopeShadowing` warning (#3219, @garyb)
- Added position information for various FFI related errors and warnings (#3276, @garyb)
- Added all available positions to `CycleInModule` and `DuplicateModule` errors (#3273, @garyb)
- Added position information for `IntOutOfRange` errors (#3277, @garyb, @kRITZCREEK)
- Warnings are now raised when a module re-exports a qualified module with implicit import (#2726, @garyb)
- `purs repl` now shows results for `:browse Prim` (#2672, @rndnoise)
- Added position information to `ErrorParsingFFIModule` (#3307, @nwolverson)
- Added position information for `ScopeConflict` cause by exports (#3318, @garyb)
- Added position information to errors that occur in binding groups and data binding groups (#3275, @garyb)
- Fixed a scoping issue when resolving operators (#2803, @kRITZCREEK, @LightAndLight)
- Type synonyms are now desugared earlier when newtype deriving (#3325, @LiamGoodacre)
- Fixed subgoals of compiler-solved type classes being ignored (#3333, @LiamGoodacre)
- Added position information to type operator associativity errors (#3337, @garyb)

**Other**

- Updated installation information to include details about prebuild binaries (#3167, @MiracleBlue)
- Test suite now prints output when failing cases are encountered (#3181, @parsonsmatt)
- Updated test suite to use tasty (#2848, @kRITZCREEK)
- Improved performance of `repl` test suite (#3234, @rndnoise)
- Refactored `let` pattern desugaring to be less brittle (#3268, @kRITZCREEK)
- Added makefile with common tasks for contributors (#3266, @bitemyapp)
- Added `ghcid` and testing commands to makefile (#3290, @parsonsmatt)
- Removed old unused `MultipleFFIModules` error (#3308, @nwolverson)
- `mod` and `div` for `Int` are no longer inlined as their definition has changed in a way that makes their implementation more complicated - purescript/purescript-prelude#161 (#3309, @garyb)
- The test suite now checks warnings and errors have position information (#3211, @garyb)
- The AST was updated to be able to differentiate between `let` and `where` clauses (#3317, @joneshf)
- Support for an optimization pass on `CoreFn` was added (#3319, @matthewleon)

## [v0.11.7](https://github.com/purescript/purescript/releases/tag/v0.11.7) - 2017-11-15

**Enhancements**

- Add position to type class declaration errors (@b123400)
- Add valid location list to orphan instance errors (@i-am-tom)
- Expand error message for UnusableDeclaration (#3088, @i-am-tom)
- Inline `Unsafe.Coerce.unsafeCoerce` (@coot)

**Bug Fixes**

- Correctly quote uppercased field labels in errors (@Thimoteus)
- `purs ide` inserts conflicting imports for types (#3131, @nwolverson)
- Instantiate abstraction body during inference to fix a type checking bug (@LiamGoodacre)
- Fix a bug related to the desugaring of nested parentheses (@MonoidMusician)
- Fix a loop in the kind checker (@paf31)
- Fix a bug in type operator precedence parsing (@paf31)
- Eliminate some redundant whitespace in the generated JS output (@matthewleon)
- Only add newline before initial group of comment lines during code generation (@brandonhamilton)
- Treat kinds as used in import warnings (@nwolverson)

**`purs ide`**

- Add an "editor mode" (@kRITZCREEK)

  When the `editor-mode` flag is specified at startup the server will not start afile watcher process any more. Instead it only reloads after successful rebuild commands. This is a lot less fragile than relying on the file system APIs, but will mean that a manual load needs to be triggered after builds that didn't go through `purs ide`.

- `purs ide` now groups `hiding` imports with implicit ones (@kRITZCREEK)
- Return documentation comments in `purs ide` completions (@nwolverson)
- Add an `actualFile` parameter to the rebuild command (@kRITZCREEK)
- Add qualified explicit import (@nwolverson)
- Fixed case-splitting on local non-exported datatypes (@LightAndLight)
- Make the `filters` parameter in the `type` command optional (@b123400)

**`purs docs`**

- Embed CSS for HTML docs (@hdgarrood)
- Fix source links for re-exports (@felixSchl)
- Use order given in export list in generated docs (@hdgarrood)
- Prevent browser from treating the title and source link as one word (@Rufflewind)
- Fix fragment links to type constructors in HTML (@hdgarrood)

**`purs repl`**

- Add `:complete` directive to `purs repl` to support completion in more editors (@actionshrimp)

**Other**

- Add docs for duplicate labels in record types (@paf31)
- Adds a document for the design of `purs ide`. (@kRITZCREEK)
- Update `PROTOCOL.md` docs for `purs ide` (@BjornMelgaard)
- Upgrade to GHC version 8.2 (@kRITZCREEK)
- Allow `blaze-html-0.9` (@felixonmars)
- Bump `Glob` dependency (@mjhoy)
- Use `Hspec` in `TestDocs` (@hdgarrood)
- Fix AppVeyor deployment (#2774) (@hdgarrood)
- Various type safety improvements to the AST (@kRITZCREEK)
- Remove some references to old executables (@hdgarrood)
- Update the installation documentation (@hdgarrood)
- Update test dependencies (@hdgarrood)
- Only build `master` and versioned tags in AppVeyor (@hdgarrood)

## [v0.11.6](https://github.com/purescript/purescript/releases/tag/v0.11.6) - 2017-07-10

**New Features**

**`RowToList` support**

(@LiamGoodacre)

There is a new type class in `typelevel-prelude` called `RowToList`, which turns
a row of types into a type-level list. This allows us to work with closed
rows in more ways at the type level. The compiler will now solve these constraints
automatically for closed rows of types.

**Enhancements**

- Allow things to be hidden from Prim (@garyb)
- Re-evaluate REPL globs on `:reload` (@hdgarrood)
- Include comments in child declarations in HTML docs (@hdgarrood)

**IDE Enhancements**

- Collect data constructors (@kRITZCREEK)
- Adds declarations for Prim (@kRITZCREEK)
- Repopulates the rebuild cache when populating volatile state (@kRITZCREEK)
- Add declaration type filter (#2924) (@sectore)
- Improve reexport bundling (@kRITZCREEK)
- Resolve synonyms and kinds (@kRITZCREEK)

**Bug Fixes**

- Replace synonyms in instance constraints (@LiamGoodacre)
- Encode PSCI's server content as UTF-8 string (@dgendill)
- Fix child declaration ordering in docs (@hdgarrood)
- Improve instance ordering in HTML docs (@hdgarrood)
- Fix links to type operators in HTML docs (@hdgarrood)

**Other**

- Add source span annotations to Declaration (@garyb)
- Add source span annotations to DeclarationRef (@garyb)
- Remove `purescript.cabal` and add to `.gitignore` (@garyb)
- Raise upper bound on `aeson` in `package.yaml` (@garyb)
- Only build master and semver tags in Travis (@hdgarrood)

## [v0.11.5](https://github.com/purescript/purescript/releases/tag/v0.11.5) - 2017-06-05

**Compiler**

**Enhancements**

**Type signatures in instances**

(@cdepillabout)

Type class instances can now include type signatures for class members, as documentation:

```purescript
data MyType = MyType String

instance showMyType :: Show MyType where
  show :: MyType -> String
  show (MyType s) = "(MyType " <> show s <> ")"
```

**Bug Fixes**

- Encode HTML content as UTF8 when using `purs repl` with `--port` (@dgendill)
- Disallow some invalid newtype-derived instances (@paf31)
- Disallow `forall` within constraints (#2874, @sectore)
- Convert `\r\n` into `\n` after reading files (@kRITZCREEK)
- Fix PSCi tests (@kRITZCREEK)
- Better variable naming hygiene in TCO. (#2868, @houli)
- Simplify TCO generated code (@matthewleon)
- Remove newlines from printed custom type errors (@matthewleon)
- Fix some `purs` command line help message issues (@Cmdv)
- Apply relative paths during pretty printing of errors (@kRITZCREEK)
- Desugar `let` properly when generating docs (@paf31)
- Fix kind signature for `RowCons` type class in documentation (@tslawler)
- Fix an issue with error messages for `TypesDoNotUnify` involving duplicate labels (#2820, @thoradam)

**Other**

- Update `package.yaml` (@sol)
- Parse support modules from actual test support `purs` (@noraesae)
- Update `build` command to run tests (@sectore)
- Bumps lower bound for `directory` (@kRITZCREEK)
- Switch `core-tests` to `psc-package` (#2830, @matthewleon)
- Small fix for the copyright dates (@seanwestfall)
- Update `CONTRIBUTING.md` for "new contributor" label (@thoradam)

**`purs ide`**

**Features**

- Add a new namespace filter (#2792, @sectore, @stefanholzmueller)

A new filter, which restricts query results to the value, type and/or kind namespaces, which allows improvements to the completion and import commands.

- Adds a command to add qualified imports (@kRITZCREEK)

This empowers editor plugins to add imports for qualified identifiers, for example in [the Emacs plugin](https://github.com/epost/psc-ide-emacs/pull/103).

- New import formatting (@kRITZCREEK)
- Group reexports in completions (@kRITZCREEK)

Editors can now choose to let `purs ide` group reexports for the same value, to reduce noise when completing values like `Data.Functor.map` which are reexported a lot and show up that many times in the completion list.

**Enhancements**

- Parse modules in parallel (@kRITZCREEK)

This can yield significant speedups in the initial load times. For example a full load of `slamdata/slamdata` improves from 11 to 6 seconds

- Introduce completion options (@kRITZCREEK)

**Bug Fixes**

- Resolve synonyms and kinds (@kRITZCREEK)
- Work around laziness when measuring command performance (@kRITZCREEK)
- Simplify state type (@kRITZCREEK)
- Extract namespace ADT (@kRITZCREEK)
- Decodes source files as UTF8 when parsing out the imports (@kRITZCREEK)
- Fix the import command for kinds (@kRITZCREEK)
- Reads files in text mode for adding imports (@kRITZCREEK)
- Add `-h`/`--help` to `ide` subcommands (@simonyangme)

## [v0.11.4](https://github.com/purescript/purescript/releases/tag/v0.11.4) - 2017-04-17

**Enhancements**

- `purs` executable will now display help text by default (@matthewleon)
- Adding `-h`/`--help` to `ide` subcommands (@simonyangme)
- Some simplifications to the tail call optimization (@matthewleon)

**Bug Fixes**

- Remove newline from printed custom type errors (@matthewleon)
- Fix pretty printing of rows in error messages (#2820, @thoradam)
- Allow user to propagate Warn constraints (@paf31)
- Match type level strings in docs renderer (#2772, @hdgarrood)
- Fix encoding bug in `purs ide` list import command (@kRITZCREEK)
- `purs ide` now reads files in text mode for adding imports (@kRITZCREEK)

**Other**

- Bump `aeson` lower bound to 1.0 (@hdgarrood)
- Add a bunch of NFData instances (@hdgarrood)
- Turn off coveralls upload for now (@paf31)
- `purs` command line help message fixes (@Cmdv)
- Switch core-tests to `psc-package` (#2830, @matthewleon)
- Update `CONTRIBUTING.md` notes (@thoradam)

## [v0.11.3](https://github.com/purescript/purescript/releases/tag/v0.11.3) - 2017-04-08

**Bug Fixes**

- Fix the exhaustivity check for pattern guards (@alexbiehl)

**Other**

- Require `directory >=1.2.3.0` for XDG support (@bergmark)
- @noraesae has refactored some PSCi code to improve the test suite.
- Use `hpack` to generate the `.cabal` file (@kRITZCREEK)
- Use XDG Base Directory Specification for `psci_history` (@legrostdg)

## [v0.11.2](https://github.com/purescript/purescript/releases/tag/v0.11.2) - 2017-04-02

**New Features**

**Polymorphic Labels**

(@paf31)

A new `RowCons` constraint has been added to `Prim`. `RowCons` is a 4-way relation between

1. Symbols
1. Types
1. Input rows
1. Output rows

which appends a new label (1) with the specified type (2) onto the front of the input row (3), to generate a new output row (4). The constraint can also be run backwards to subtract a label from an output row.

This allows us to quantify types over labels appearing at the front of a row type, by quantifying over the corresponding symbol/type pair. This gives us a limited form of polymorphic labels which enables things like writing [a single lens for any record accessor](https://github.com/purescript/purescript/blob/e4ff177017f1411ad4cbeade129cfe1bb52d6e99/examples/passing/PolyLabels.purs#L41-L51).

**Enhancements**

- Use XDG Base Directory Specification for the location of the `psci_history` file (@legrostdg)
- Collect more information for classes and synonyms in `purs ide` (@kRITZCREEK)

**Bug Fixes**

- Desugar pattern guards *after* type checking, to avoid an issue with the exhaustivity checker (@alexbiehl)

**Other**

- A new PSCi evaluation test suite was added (@noraesae)
- Use `hpack` to generate the `.cabal` file (@kRITZCREEK)

## [v0.11.1](https://github.com/purescript/purescript/releases/tag/v0.11.1) - 2017-03-28

**Bug Fixes**

**Compiler**

- Enable TCO for variable intros and assignments #2779 (@paf31)
- Fixed special case in codegen for guards #2787 (@paf31)

**Docs generation**

- Wrap decl title in span for better double-click selection #2786 (@rightfold)
- List instance info under correct sections, fix #2780 (@paf31)

## [v0.11.0](https://github.com/purescript/purescript/releases/tag/v0.11.0) - 2017-03-25

This release includes several breaking changes, in preparation for the 1.0 release, as well as many enhancements and bug fixes.

Most users will probably want to wait until all aspects of the release have been finalized. Progress on libraries and tools is being tracked [here](https://github.com/purescript/purescript/issues/2745).

Many thanks to the contributors who helped with this release!

**Breaking Changes**

(@garyb, @paf31)

**`=>` now acts like a binary type operator**

It was previously possible to specify many constraints in the same context by
separating them with commas inside parentheses on the left of the `=>`:

```purescript
runFreeT :: ∀ m f. (Functor f, Monad m) => ...
```

This is no longer allowed. Instead, `=>` now acts like a binary operator, with a
constraint on the left and a type on the right. Multiple constraints must be
introduced using currying, as with regular function arguments:

```purescript
runFreeT :: ∀ m f. Functor f => Monad m => ...
```

This is in preparation for adding _constraint kinds_, at which point `=>` will become
an actual binary type operator, defined in `Prim`.

**`*` and `!` kinds have been removed**

The kind symbols `*` (for the kind of types) and `!` (for the kind of effects) have been
removed from the parser. Instead of `*`, use `Type`, which is defined in `Prim`.
Instead of `!`, use `Effect`, which can now be imported from `Control.Monad.Eff`.

The `#` symbol, which is used to construct a row kind, is still supported. We cannot move this kind into `Prim` (because it is polykinded, and we do not support kind polymorphism).

**One single consolidated executable**

The various `psc-*` executables have been replaced with a single executable called `purs`.
The various subcommands are documented on the `--help` page:

```
bundle     Bundle compiled PureScript modules for the browser
compile    Compile PureScript source files
docs       Generate Markdown documentation from PureScript source files
hierarchy  Generate a GraphViz directed graph of PureScript type classes
ide        Start or query an IDE server process
publish    Generates documentation packages for upload to Pursuit
repl       Enter the interactive mode (PSCi)
```

Wrapper scripts will be provided in the binary distribution.

**`psc-package` was removed**

`psc-package` has been removed from the main compiler distribution. It will still
be maintained along with the package sets repo, but will not be bundled with the compiler.

A binary distribution which is compatible with this release is [available](https://github.com/purescript/psc-package/releases/tag/v0.1.0).

**Implicitly discarded values in `do` blocks now raise errors**

Code which discards the result of a computation in a `do` block:

```purescript
duplicate :: Array a -> Array a
duplicate xs = do
  x <- xs
  [true, false] -- the result here is discarded
  pure x
```

will now raise an error. The compiler allows values of certain types to be discarded,
based on the `Discard` class in `Control.Bind`. The only type which can be discarded is
`Unit`, but the feature was implemented using a type class to enable support for
alternative preludes.

**No more dependency on the Bower executable**

In addition to removing `psc-package` from the compiler distribution, we have also
removed any explicit dependency on the Bower executable. The compiler will not assume
use of any particular package manager, but will aim to provide generic support for
package managers generally, via command line options and hooks.

`purs publish` will continue to use the Bower JSON formats. The `bower.json` format
is now referred to as the "manifest file", while the output of `bower list --json`,
which is used by `purs publish` internally, is referred to as the "resolutions file".

**Enhancements**

**Pattern Guards**

(@alexbiehl)

In addition to regular guards:

```purescript
foo x | condition x = ...
```

the compiler now supports _pattern guards_, which let the user simultaneously
test a value against a pattern, and bind names to values.

For example, we can apply a function `fn` to an argument `x`, succeeding only if
`fn` returns `Just y` for some `y`, binding `y` at the same time:

```purescript
bar x | Just y <- fn x = ... -- x and y are both in scope here
```

Pattern guards can be very useful for expressing certain types of control flow when
using algebraic data types.

**HTML Documentation**

(@hdgarrood)

The `--format html` option has been added to `purs docs`. The HTML format uses
the Pursuit template, and is very useful for rendering documentation for offline
use.

[Here is an example](http://harry.garrood.me/purs-html-docs-example/) of the generated HTML.

**Duplicate Labels**

(@paf31)

Row types now support duplicate labels, which can be useful when using the `Eff`
monad. For example, we could not previously use the `catchException` function if
the resulting action _also_ required the `EXCEPTION` effect, since otherwise the
type of the inner action would contain a duplicate label.

Rows are now unordered collections (of labels and types) _with duplicates_. However,
the collection of types for a specific label within a row _is_ ordered.
Conceptually, a row can be thought of as a type-level `Map Label (NonEmptyList Type)`.

A type constructor (such as `Record`) which takes a row of types as an argument should
define what its meaning is on each row. The meaning of a value of type `Record r`
is a JavaScript object where the type of the value associated with each label is given
by the head element of the non-empty list of types for that label.

**Row Constraints**

(@doolse, @paf31)

A new constraint called `Union` has been added to `Prim`. `Union` is a three-way relation between
rows of types, and the compiler will solve it automatically when it is possible to do so.

`Union` is a left-biased union of rows which takes into account duplicate labels. If the same label appears in rows `l` and `r`, and `Union l r u` holds, then the label will appear twice in `u`.

`Union` makes it possible to give a type to the function which merges two records:

```purescript
merge :: forall r1 r2 r3. Union r1 r2 r3 => Record r1 -> Record r2 -> Record r3
```

Note that this is a left-biased merge - if the two input record contain a common label, the type of the
label in the result will be taken from the left input.

**Patterns in `let` expressions**

(@noraesae)

Let expressions and `where` clauses can now use binders on the left hand side of
a declaration:

```purescript
map f xs =
  let { head, tail } = uncons xs
  in [f head] <> map f tail
```

Unlike in Haskell, declarations with these patterns cannot appear in dependency cycles, and bound names can only be used in declarations after the one in which they are brought into scope.

**Find record accessors in Type Directed Search**

(@kRITZCREEK)

Type-directed search will now include results for record accessors. This can
be very useful when working with extensible records with a type-driven programming
workflow.

**Other Enhancements**

- Add basic usability check and error for ambiguously-typed type class members (@LiamGoodacre)
- Improved skolem escape check (@paf31)
- Fix links to declarations in `Prim` (@hdgarrood)
- Emit `_` instead of `false` case for `if then else` to improve optimizations (@rightfold)
- Add `InvalidDerivedInstance` error to improve errors for derived instances (@paf31)
- Make generated code for superclass instances less ugly (@paf31)
- Support polymorphic types in typed binders (@paf31)
- Make file paths relative in error messages (@paf31)
- Improve errors from module sorter (@paf31)
- Improve error for unused type variables (@paf31)
- Include source span in externs file for error reporting purposes (@paf31)
- Improve instance arity errors (@mrkgnao)

**`purs ide`**

**Features**

**Improve import parsing**

- `purs ide` now uses a new import parser, which allows `purs ide` to handle any
import section that the compiler would accept correctly. (@kRITZCREEK)
- Parse imports with hanging right paren (@matthewleon)
- Reuses lenient import parsing for the list import command (@kRITZCREEK)

**Don't create the output/ directory if it can't be found**

(@kRITZCREEK)

`purs ide` will now no longer leave empty output/ directories behind when it is
started in a directory that is not a PureScript project.

**Collect type class instances**

(@kRITZCREEK)

`purs ide` collects instances and stores them with their respective type class.
There's no way to retrieve these yet, but we will extend the protocol soon.

**Bug Fixes**

- No longer strip trailing dots for Pursuit queries (@kRITZCREEK)
- Fix #2537 (`psc-ide` shouldn't crash when building a non-existent file) (@kRITZCREEK)
- Fix #2504 (fix a crash related to prematurely closed handles) (@kRITZCREEK)
- Speed up rebuilding by x2, by rebuilding with open exports asynchronously (@kRITZCREEK)
- Return operators in `purs ide` imports list (@nwolverson)
- Also detect location information for operators (@kRITZCREEK)

**Cleanup**

- Removes unnecessary clause in import pretty printing (@kRITZCREEK)
- Removes the deprecated `--debug` option (@kRITZCREEK)
- Restructure testing to avoid running the server (@kRITZCREEK)

**`purs repl`**

- Add back `.purs-repl` file support (@paf31)
- PSCi command changes, add `:clear` (@noraesae)
- Declarations no longer require `let` (@noraesae)
- Improve CLI error and startup messages (@noraesae)

**Bug Fixes**

- Changes to help the tail call optimization fire more consistently (@paf31)
- Fix `everythingWithScope` traversal bug #2718 (@paf31)
- Errors for open rows in derived instances (@paf31)
- Instantiate types in record literals as necessary (@paf31)
- Fix `Generic` deriving with synonyms (@paf31)
- Rebuild modules if necessary when using `--dump-corefn` (@paf31)
- Ensure solved type classes are imported (@LiamGoodacre)
- Allow for older Git versions in `purs publish` (@mcoffin)
- Fix `purs publish --dry-run` (@hdgarrood)
- Exported data constructors can now contain quotes (@LiamGoodacre)

**Documentation**

- Capitalise *script into *Script (@noraesae)

**Performance**

- Optimize `keepImp` (@paf31)
- Replace `nub` with `ordNub` (@matthewleon)
- Combine inlining optimizations into a single pass (@paf31)

**Other**

- Add `HasCallStack` to internalError (@alexbiehl)
- Use Stackage LTS 8.0 (@noraesae)
- Address Travis timeout issues (@hdgarrood)
- Improve module structure in PSCi test suite (@noraesae)
- Fix the PSCi script (@mrkgnao)
- Include Git commit information in non-release builds (@hdgarrood)
- Add test case for #2756 (@int-index)
- Some code cleanup in the module imports phase (@matthewleon)

## [v0.10.7](https://github.com/purescript/purescript/releases/tag/v0.10.7) - 2017-02-11

This release contains a bug fix for a bug in `psc-bundle` which was introduced in 0.10.6.

## [v0.10.6](https://github.com/purescript/purescript/releases/tag/v0.10.6) - 2017-02-07

**Enhancements**
- Add support for user defined warnings via the `Warn` type class (@LiamGoodacre, [blog post](https://liamgoodacre.github.io/purescript/warnings/2017/01/17/purescript-warn-type-class.html))
- Support nested record update (@LiamGoodacre, [blog post](https://liamgoodacre.github.io/purescript/records/2017/01/29/nested-record-updates.html))
- Inline `unsafePartial` (@paf31)
- Fail early when `bind` is brought into scope inside `do` (@paf31)

**Bug Fixes**
- Disallow polymorphic types in binders, preventing a crash (@paf31)
- Rebuild modules if necessary when using `--dump-corefn` (@paf31)
- `TypeLevelString`/`TypeConcat` should not be quoted (@michaelficarra)
- Generate JS static member accesses whenever possible (@michaelficarra)
- Require dependencies to exist during sorting phase (@paf31)
- Fix inlining for `negateInt` (@paf31)
- Fix object key quoting (@hdgarrood)
- Don't expand synonyms until after kind checking (@paf31)
- Fix 'Unknown type index' on mismatch between class and instance argument counts (@LiamGoodacre)
- Style comment types differently (@matthewleon)

**`psc-ide`**
- Return operators in `psc-ide` imports list (@nwolverson)
- Collect type class instances (@kRITZCREEK)
- Log failing to accept or parse an incoming command (@kRITZCREEK)
- Fix #2537 (@kRITZCREEK)
- Fix #2504 (@kRITZCREEK)
- Also detect location information for operators (@kRITZCREEK)
- Speeds up rebuilding by x2 (@kRITZCREEK)
- Restructure testing to avoid running the server (@kRITZCREEK)

**`psc-publish`**
- Add modules for rendering HTML documentation (@hdgarrood)
- Fix `psc-publish --dry-run` (@hdgarrood)
- Fix failure to parse git tag date in `psc-publish` (@hdgarrood)
- Add git tag time to `psc-publish` JSON (@hdgarrood)
- Remove `Docs.Bookmarks` (@hdgarrood)

**Performance**
- Combine inlining optimizations into a single pass (@paf31)
- Use `Map.foldlWithKey'` instead of `foldl` (@hdgarrood)
- Minor memory usage improvements in `Language.PureScript.Docs` (@hdgarrood)

**Other**
- Generate data constructors without IIFEs (@hdgarrood)
- Add stack-ghc-8.0.2.yaml (@noraesae)
- Add `HasCallStack` to `internalError` (@alexbiehl)
- Update `psc-package` to use turtle 1.3 (@taktoa)
- Remove `JSAccessor`; replace with `JSIndexer` (@michaelficarra)
- Store more information in `RenderedCode` (@hdgarrood)

## [v0.10.5](https://github.com/purescript/purescript/releases/tag/v0.10.5) - 2017-01-06

**Enhancements**
- Adds specific error message when failing to import bind (@FrigoEU)

**Bug Fixes**
- Detect conflicting data constructor names (@LiamGoodacre)
- Update pretty printer for Kinds (@hdgarrood)
- Restore JSON backwards compatibility for `PSString` (@hdgarrood)
- Replace type wildcards earlier (@paf31)
- Restore backwards compatibility for parsing Kinds (@hdgarrood)

**Other**
- Update `bower-json` to 1.0.0.1 (@hdgarrood)

## [v0.10.4](https://github.com/purescript/purescript/releases/tag/v0.10.4) - 2017-01-02

**New Features**

**Deriving `Functor`**

(@LiamGoodacre, #2515)

The `Functor` type class can now be derived using the standard `derive instance` syntax:

``` purescript
newtype F a = F { foo :: Array a, bar :: a }

derive instance functorF :: Functor F
```

**User-Defined Kinds**

(@LiamGoodacre, #2486)

Custom kinds can now be defined using the `foreign import kind` syntax:

``` purescript
foreign import kind SymbolList
```

Custom kinds can be ascribed to types using `foreign import data` declarations, as usual:

``` purescript
foreign import data Nil :: SymbolList
foreign import data Cons :: Symbol -> SymbolList -> SymbolList
```

Note that kind arguments are not supported.

User defined kinds can be imported/exported using the `kind` prefix, for example:

``` purescript
import Type.SymbolList (kind SymbolList)
```

**Source Maps in `psc-bundle`**

(@nwolverson)

`psc-bundle` will now generate source maps if the`--source-maps` flag is used.

**Solving `CompareSymbol` and `AppendSymbol`**

(@LiamGoodacre, #2511)

Support for the new `purescript-typelevel-prelude` library has been added to the compiler. `CompareSymbol` and `AppendSymbol` constraints will now be solved automatically for literal symbols.

**New `psc-package` Features**

(@paf31)

Two new commands have been added to `psc-package` to support library authors and package set curators.
- The `updates` command (#2510) is used to update packages in the set.
- The `verify-set` command (#2459) is used to verify the health of a package set. This command replicates the work done by the `package-sets` CI job, and can be used to test modifications to the package set locally before making a pull request.

**Enhancements**
- Update orphan instance check to use covering sets when functional dependencies are involved (@LiamGoodacre)
- Add `--node-path` option to PSCi to modify the path to the Node executable (#2507, @paf31)
- Add package information to re-exports (@hdgarrood)
- Add `Prim` docs to the library (#2498, @hdgarrood)

**Bug Fixes**
- Derive instances when data types use type synonyms (#2516, @paf31)
- Unwrap `KindedType` when instance solving (@LiamGoodacre)
- Update links to wiki (#2476, @LiamGoodacre)
- Update websocket host to fix PSCi on Windows (#2483, @seungha-kim)
- Fix `psc-ide` tests on windows (@kRITZCREEK)
- Fix some issues with the pretty printer (#2039, @paf31)

**Other**
- More robust license generator script (@hdgarrood)
- Further conversions to `Text` in the `Docs` modules (#2502, @hdgarrood)
- Add upper bound on `turtle`, fixes #2472, (@hdgarrood)
- Fix version bounds on `language-javascript` (@hdgarrood)

## [v0.10.3](https://github.com/purescript/purescript/releases/tag/v0.10.3) - 2016-12-11

**Enhancements**

**Solving `IsSymbol` instances**

(@LiamGoodacre)

The compiler will now derive `Data.Symbol.IsSymbol` instances for type-level string literals.

This enables interesting type-level programming features, such as [deriving `Show` instances using `Data.Generics.Rep`](https://asciinema.org/a/1lc5nn3o9b24y2bos8eowmfa9).

**Rows in Instance Heads**

(@LiamGoodacre)

The compiler now allows rows to appear in type class instance heads, but only in type arguments which are fully determined by some functional dependency.

This allows instances like

``` purescript
MonadState { field :: Type } MyAppMonad
```

and also `Newtype` instances for newtypes which contain records.

**Speeds up parsing by reading files as Text**

(@kRITZCREEK)

The use of `String` has been replaced by `Text` in the compiler, resulting in some non-trivial performance improvements.

**Functional Dependencies in `psc-docs` output**

(@soupi, #2439)

`psc-docs` now includes functional dependency information when rendering type classes.

**New `psc-package` Commands**
- The `available` command (@andyarvanitis) shows all available packages in the current package set
- The `uninstall` command (@joneshf) removes a package from the set of active packages and updates the package configuration file.

**Type Class Warning (@joneshf)**

A warning was added for shadowed type variables in type class declarations.

**Bug Fixes**
- `psc-package`: display full path in 'packages.json does not exist' error messsage (@andyarvanitis)
- Use `writeUTF8File` in `psc-bundle` (@hdgarrood)
- Use HTTPS to query Pursuit (@paf31)
- Moved the expansion of astral code points to UTF-16 surrogate pairs from the JS code generator to the parser (@michaelficarra, #2434)
- Allow astral code points in record literal keys (@michaelficarra, #2438)
- Add value source positions (@nwolverson)
- Update error message of `ErrorInDataBindingGroup` to include participating identifiers (@LiamGoodacre)

**`psc-ide`**
- Polling option for psc-ide-server (@kRITZCREEK)
- Better logging and diagnostics (@kRITZCREEK)

**Other**
- Dump output of `psc` tests to file (@andyarvanitis, #2453)
- Fix windows CI (@hdgarrood)
- Link to new documentation repo (@hdgarrood)
- Create documentation for psc-package (@paf31)
- Fix GHC 8.0.2 build (@RyanGlScott)
- Add `psc-package` to release bundle (@marsam)
- Update for latest `language-javascript` (@tmcgilchrist)
- Fix exhaustivity warnings (@charleso)
- Update `CONTRIBUTING.md` (@osa1)

## [v0.10.2](https://github.com/purescript/purescript/releases/tag/v0.10.2) - 2016-11-07

**Major Changes**

**Type-directed search (@kRITZCREEK)**

This extends the typed holes error messages to include suggested replacements for a typed hole, by using type subsumption to determine which identifiers in scope are appropriate replacements.

A blog post will accompany this feature soon.

**`psc-package` (@paf31)**

This is an experimental package manager for PureScript packages. It supports the following commands:
- `init` - create a new project using the package set for the current compiler version
- `update` - sync the local package collection with the package set
- `install` - install a specific package from the current set and add it to the package config
- `build` - run `psc` on any active packages
- `sources` - list source globs for active package versions
- `dependencies` - list transitive dependencies of the current project

For example:

``` text
$ psc-package init
$ psc-package install transformers
$ psc-package build
```

Eventually, `psc-package` might replace the use of Bower, but that will require support from tools like Pulp. For now, package authors should continue to publish packages using Bower and Pursuit.

**`Data.Generic.Rep.Generic` Deriving (@paf31)**

This is an alternative generic programming implementation based on `GHC.Generics`. It should allow deriving of more interesting classes, such as `Semigroup`. See the `purescript-generics-rep` package for examples.

**Enhancements**
- #2323: Sort IDE-generated explicit imports (@bbqbaron)
- #2374: Add error message for ambiguous type variables in inferred contexts (@bbqbaron)
- #934 Add paste mode, remove --multi-line option (@paf31)
- Allow symbols in data constructors (@brandonhamilton)
- Fix inliner for integer bitwise operators (@brandonhamilton)
- Use SSL for pursuit queries (@guido4000)

**Bug Fixes**
- #2370, allow rows in instance contexts (@paf31)
- #2379, add error message for unknown classes (@paf31)
- Better error messages for bad indentation (@paf31)
- Fix inliner for `Data.Array.unsafeIndex` (@brandonhamilton)
- Fix issue with typed holes in inference mode (@paf31)
- Fix scope traversal for do-notation bind. (@LiamGoodacre)
- Handle `TypeLevelString` when checking orphans (@joneshf)
- Move unsafeIndex to Data.Array (@brandonhamilton)
- Pretty-print suggested types differently (@paf31)
- Traversal should pick up bindings in all value declarations. (@LiamGoodacre)
- Treat type annotations on top-level expressions as if they were type declarations (@paf31)

**Other**
- Refactor subsumes function (@paf31)
- Refactor to use `lens` (@kRITZCREEK)
- Small cleanup to `Language.PureScript.Interactive.IO` (@phiggins)
- Speeds up parsing by reading files as `Text` (@kRITZCREEK)
- Update outdated comments about Prim types (@rightfold)

## [v0.10.1](https://github.com/purescript/purescript/releases/tag/v0.10.1) - 2016-10-02

**Breaking Changes**

The new functional dependencies feature fixes type inference in some cases involving multi-parameter type classes. However, due to a bug in the compiler, some of those expressions were previously type checking where they should not have. As a result, it is necessary to add functional dependencies to some classes in order to make previous code type-check in some cases. Known examples are:
- `MonadEff` and `MonadAff`
- `MonadState`, `MonadReader`, and the rest of the MTL-style classes in `transformers`

**New Features**

**`Data.Newtype` Deriving**

(@garyb)

It is now possible to derive the `Newtype` class for any data declaration which is a `newtype`, using the existing `deriving instance` syntax:

``` purescript
newtype Test = Test String

derive instance newtypeTest :: Newtype Test _
```

Note that the second type argument should be specified as a wildcard, and will be inferred.

**Added type level string functions**

(@FrigoEU)

The `Prim` module now defines the `TypeString` and `TypeConcat` type constructors, which can be used to build more descriptive error messages which can depend on types, using the `Fail` constraint:

``` purescript
instance cannotShowFunctions
    :: Fail ("Function type " <> TypeString (a -> b) <> " cannot be shown.")
    => Show (a -> b) where
  show _ = "unreachable"

infixl 6 type TypeConcat as <>
```

**`--dump-corefn`**

(@rightfold)

The compiler now supports the `--dump-corefn` option, which causes the functional core to be dumped in `output/**/corefn.json`. This should be useful for implementing new backends which interpret the functional core.

**Newtype Deriving**

(@paf31)

It is now possible to derive type class instances for `newtype`s, by reusing the instance for the underlying type:

``` purescript
newtype X = X String

derive newtype instance showX :: Show X
```

Note that it is possible to derive instances for multi-parameter type classes, but the newtype must only appear as the last type argument.

**Allow anonymous accessor chains (`_.a.b`)**

(@rvion)

Anonymous record accessor syntax has been extended to work with chains of one or more accessors:

``` purescript
getBaz = _.foo.bar.baz
```

**Functional Dependencies (@paf31)**

The type class solver now supports functional dependencies. A multi-parameter type class can define dependencies between its type arguments by using the `->` operator:

``` purescript
class Stream el s | s -> el where
  cons :: el -> (Unit -> s) -> s
  uncons :: s -> { head :: el, tail :: s }
```

Here, the `s` and `el` type arguments are related by a single functional dependency, which ensures that there is at most one instance for any given type `s`. Alternatively, the type `s` _determines_ the type `el`, i.e. there is an implicit function from types `s` to types `el`. This information can be used by the solver to infer types where it was previously not possible.

See the following examples for more information:
- [Streams](https://github.com/purescript/purescript/blob/f5aa07606b2ed87343bb80244c5490cb157def0a/examples/passing/Stream.purs)
- [GHC-style generics](https://github.com/purescript/purescript/blob/f5aa07606b2ed87343bb80244c5490cb157def0a/examples/passing/GHCGenerics.purs)
- [Type-level arithmetic](https://github.com/purescript/purescript/blob/f5aa07606b2ed87343bb80244c5490cb157def0a/examples/passing/FunWithFunDeps.purs)
- [Heterogeneous Lists](https://gist.github.com/paf31/ded46a2fb2419f4610582a02a0690bec)

**Enhancements**
- Return qualifier from explicit/hiding imports (@nwolverson)
- Verify entry points exist in `psc-bundle` (@kRITZCREEK)
- Improved error messages for record subsumption (@FrigoEU)

**psc-ide**
- Resolve types/kinds for operators (@kRITZCREEK)
- Unify Completion Commands (@kRITZCREEK)
- Parse type annotations from source files (@kRITZCREEK)
- Update pursuit JSON parsing (@nwolverson)
- Remove a pursuit workaround (@kRITZCREEK)
- Add a suggestion to the `UnusedDctorImport` warning (@FrigoEU)
- Return JSON errors for cycles in module dependencies (@kRITZCREEK)

**Bug Fixes**
- Fix usage detection for operators (@garyb)
- Fix handling of duplicate module imports in JS codegen (@garyb)
- Fix a small bug in the type pretty-printer (@paf31)
- Fix function application judgment (@paf31)
- Fix inlining for `$` and `#` operators (@garyb)
- Fix `everywhereOnTypesTopDown` (@ianbollinger)
- Fix unification of string literals (@paf31)

**Infrastructure**
- Support `aeson-1.0` (@phadej)
- Support `http-client-0.5` (@phadej)
- Safer installation from source in INSTALL.md (@hdgarrood)

**Implementation**
- Fix most HLint warnings (@ianbollinger)
- Fixing imports (@charleso)
- Export `desugarDecl` from `Sugar.ObjectWildcards` (@rvion)
- Remove legacy `ObjectGetter` and update doc (@rvion)

## [v0.9.3](https://github.com/purescript/purescript/releases/tag/v0.9.3) - 2016-08-01

**Enhancements**
- Better context information for typed hole errors (@paf31)
- Improved error messages in the constraint solver. Type class errors now include better contextual information, including smaller source spans. (@paf31)

**Bug Fixes**
- Decode externs with correct encoding (@natefaubion)
- Fix bad codegen for empty string fields (@LiamGoodacre, #2244)
- Instantiate types in array literals before unification (@paf31, #2252)

**Other**
- Upgrade to protolude 0.1.6 (@ilovezfs)
- Use latest LTS (@paf31, #2241)
- Add upper bound to http-client (@paf31, #2237)
- Combine the sdist and coverage builds. Avoid .tix files during deployment. (@paf31)

## [v0.9.2](https://github.com/purescript/purescript/releases/tag/v0.9.2) - 2016-07-11

**Enhancements**

**Goto Definition**

@kRITZCREEK has added the ability to return position information for expressions in `psc-ide`. This can be used to implement a Goto Definition feature in IDEs which use `psc-ide-server` as the backend.

**Evaluate PSCi expressions in the browser**

(@paf31)

PSCi now features an alternative backend, which can run commands in the browser via a websocket. To use this mode, simply pass the `--port` option on the command line:

```
$ pulp psci --port 9000
```

and open your web browser to `localhost` on that port.

See https://github.com/paf31/psci-experiment for a demonstration.

**`psc-ide` architecture changes**

@kRITZCREEK has worked on changing the architecture of `psc-ide` generally, to load data in multiple phases and asynchronously. This enables new features like Goto Definition above.

**Other**
- Allow `pipes` version 4.2 (@felixonmars)
- Elaborate re-exports (@garyb)

**Bug Fixes**

**`psc-ide`**
- Fix unicode encoding of json responses (@kRITZCREEK)
- Improved handling of reexports (@kRITZCREEK)

**Other**
- Update Data.Function constant for prelude 1.0 (@felixSchl)
- Include position info in ScopeShadowing warning (@garyb)

## [v0.9.1](https://github.com/purescript/purescript/releases/tag/v0.9.1) - 2016-06-01

PureScript 0.9.1 is a major stable release of the compiler. It removes features which were deprecated in the 0.8.x series, and contains several useful enhancements and bug fixes.

This release will be accompanied by new releases of the core libraries and a compatible version of Pulp, which have been updated to work with this version.

Due to the relatively large number of breaking changes, library authors are advised that they will probably need to update their libraries to maintain compatibility. Users may prefer to continue using version 0.8.5 until their dependencies have been updated.

**Breaking Changes**

**Name resolving**

(@garyb)

The way names are resolved has now been updated in a way that may result in some breakages. The short version is: now only names that have been imported into a module can be referenced, and you can only reference things exactly as you imported them.

Some examples:

| Import statement | Exposed members |
| --- | --- |
| `import X` | `A`, `f` |
| `import X as Y` | `Y.A` `Y.f` |
| `import X (A)` | `A` |
| `import X (A) as Y` | `Y.A` |
| `import X hiding (f)` | `A` |
| `import Y hiding (f) as Y` | `Y.A` |

Qualified references like `Control.Monad.Eff.Console.log` will no longer resolve unless there is a corresponding `import Control.Monad.Eff.Console as Control.Monad.Eff.Console`. Importing a module unqualified does not allow you to reference it with qualification, so `import X` does not allow references to `X.A` unless there is also an `import X as X`.

Although the new scheme is stricter it should be easier to understand exactly what the effect of any given import statement is. The old resolution rules for qualified names were obscure and unexpected results could arise when locally-qualified module names overlapped with "actual" module names.

Module re-exports have also been tightened up as a result of these rules. Now if module `X` is only imported `as Y`, the re-export must list `module Y` also. If a module is imported without being re-qualified then the original name is used.

**Partial Constraints**

(@garyb, @paf31)

The compiler will now generate an error for a missing `Partial` constraints, where it would previously have issued a warning.

**Module Restrictions**

(@garyb, @paf31)
- Imports must now appear before other declarations in a module.
- A source file must now contain exactly one module.

These restrictions will allow us to improve incremental build times in future, since we will only need to parse a small prefix of each file in order to figure out what needs to be rebuilt. Right now, we need to parse every file fully.

**Foreign Function Interface Changes**

(@paf31)

Foreign modules are now found by filename rather than by searching for a custom JavaScript comment. The foreign module is found by changing the extension of the corresponding PureScript module from `.purs` to `.js`.

This change was made to be more consistent with `psc-ide`, and also to adopt a simple convention which will port well to other backends.

**Operator Aliases**

(@garyb)

All operators must be defined as aliases from now on. That is, it is no longer valid to define an operator as a name in local scope (e.g. `let (#) x y = x y in ...`). This change makes it possible to generate better JavaScript code for operators, by desugaring them to the functions they alias.

**Other**
- Deprecated class import/export syntax has been removed (@LiamGoodacre). Classes are now imported using the `class` keyword, and exported similarly:

  ``` purescript
  import Prelude (class Show, show)
  ```
- Remove support for `=` in record binders (@paf31).

  Record binders such as

  ``` purescript
  f { x = 0 } = true
  ```

  are no longer supported. Record binders must now use `:` instead:

  ``` purescript
  f { x: 0 } = true
  ```
- `Prim.Object` has been renamed to `Prim.Record` (#1768, @paf31)

**Enhancements**

**Programmable Type Errors**

(@paf31)

Constraints can now contain type-level strings which can be used as custom error messages using the `Fail` constraint. For example, one can now document the fact that foreign types such as `JSDate` cannot be made instances of `Generic`:

``` purescript
instance dateIsNotGeneric
  :: Fail "JSDate is not Generic. Consider using Int with toEpochMilliseconds instead."
  => Generic JSDate where
    fromSpine   = crashWith "fromSpine: unreachable"
    toSpine     = crashWith "toSpine: unreachable"
    toSignature = crashWith "toSignature: unreachable"
```

Attempting to derive a `Generic` instance for a type containing `JSDate` will then result in

``` text
A custom type error occurred while solving type class constraints:

    JSDate is not Generic. Consider using Int with toEpochMilliseconds instead.
```

**Typed Hole Improvements**

(#2070, @paf31)

Typed hole error messages now include the types of any names in scope, to assist with type-driven development:

``` text
> :t \x -> maybe 0 ?f x
Error found:
in module $PSCI
at  line 1, column 8 - line 1, column 22

  Hole 'f' has the inferred type

    t0 -> Int

  in the following context:

    it :: Maybe t0 -> Int
    x :: Maybe t0


in value declaration it

where t0 is an unknown type
```

**Editor Support**
- The results of the last rebuild are now cached by `psc-ide`, which improves completion support for editor plugins. (@kRITZCREEK)
- A `reset` command was added to `psc-ide` (@kRITZCREEK)
- The compiler will now suggest replacements to address `MissingTypeDeclaration` and `TypeWildCard` warnings (@nwolverson)

**PSCi Improvements**

(@paf31)
- The design of PSCi has been changed to improve performance. PSCi now precompiles all dependencies and uses the same incremental rebuilding approach as `psc-ide`. This means that the `:load` and `:foreign` commands have been removed, since dependencies are fixed and pre-compiled when PSCi loads.
- PSCi now supports alternative base libraries such as Neon, by depending on `purescript-psci-support` for its supporting code.

**Colors in Error Messages**

Types and values will now be highlighted in error messages, when the terminal supports it (MacOS and Linux for now) (@soupi).

**Type Names**

Prime characters are now allowed in type names. (@garyb)

**Bug Fixes**
- Parser error messages inside type class and instance declarations were improved (#2128, @bmjames)
- Editor suggestions for imports now use `(..)` (@garyb)
- Source-spans to token end position (@nwolverson)
- Some pretty printing issues related to string literals in records were fixed (@LiamGoodacre)
- Some presentation bugs in PSCi's `:show import` were fixed (@LiamGoodacre)
- Parsec was updated to the latest version to fix an issue with literal parsing (#2115, @hdgarrood)
- Fixed a bug related to certain typed binders which would cause the compiler to crash (#2055, @paf31)
- As-patterns now bind less tightly (@paf31)
- More identifiers can now be parsed in FFI imports (@michaelficarra)
- Fixed a performance issue which manifested under certain conditions in `psc-ide` (#2064, @kika)
- Fixed a test which contained an unreliable comparison (#2093, @andyarvanitis)
- The precedence of type application was corrected (#2092, @paf31)
- An indentation bug in the parser was fixed (@DavidLindbom)
- License errors from `psc-publish` were improved (@hdgarrood)

**Other**
- The test suite now exercises various compiler warnings (@garyb)
- The test suite performance was improved by using incremental rebuilds (@paf31)
- The test suite now tests that passing tests contain a `main` function (@hdgarrood)
- The test suite now supports tests which use multiple files (@garyb)
- Portability of the core library test suite was improved (@bmjames)
- Performance of import elaboration was improved (@garyb)
- We now use Stack for our CI builds and release builds (#1974, @hdgarrood)
- We now use `NoImplicitPrelude` and enable some global extensions (@garyb)
- Type-safety in the source-level AST was improved (@garyb)
- Use HSpec for the compiler tests (@garyb)
- New Prelude names in 0.9 (@garyb)

## [v0.9.0](https://github.com/purescript/purescript/releases/tag/v0.9.0) - 2016-05-22

**This is pre-release software**

This release is provided so that library developers can test the new compiler features.

## [v0.8.5](https://github.com/purescript/purescript/releases/tag/v0.8.5) - 2016-04-21

**New Features**
- Fast recompilation for single files in `psc-ide-server` #1712 (@kRITZCREEK, @paf31)

  The [`pscid`](https://github.com/kRITZCREEK/pscid) project makes use of this to watch files as you work and raise errors and warnings when they occur with near instant feedback.
- Operator aliases can now be declared for types #416 (@garyb)

  ``` purescript
  infixr 6 type Natural as ~>
  ```
- Underscore wildcards can now be used in `case` and `if` expressions #1558 (@garyb)

  ``` purescript
  case _ of
    Something -> ...
  ```

  ``` purescript
  -- underscores can optionally be used in any part of an `if` expression
  cond = if _ then _ else _
  picker = if _ then "x" else "y"
  ```
- Typed holes #1283 (@garyb)

  ``` purescript
  example :: forall a. Maybe a -> Unit
  example ma = ?umm
  ```

  ```
  Hole 'umm' has the inferred type

    Unit

  in value declaration example
  ```

  You can use any identifier name after the question mark and that will be used to label the hole in the raised error message.

**Breaking changes**
- Type annotations may need parentheses in some situations that they previously did not due to the introduction of type operators. For example, `x :: a == y` will be now parsed as `x :: (a == y)` instead of `(x :: a) == y`.

**Enhancements**
- Improved error messages for invalid FFI identifiers #2011 (@hdgarrood)
- `psc-publish` now allows publishing of packages with a valid SPDX license field in `bower.json` #1985 (@hdgarrood)
- Haddock markdown fix #2001 (@trofi)
- `psc-ide` now creates the `output` folder on startup if it is missing #2030 (@kRITZCREEK)

**Bug Fixes**
- Fixed an issue with incorrect suggestions when re-exporting modules #1862 (@garyb)
- Fixed an issue with invalid redundant import warnings #1823 (@garyb)
- Fixed an issue where `DuplicateSelectiveImport` would not fire when it should #2004 (@garyb)
- Fixed the error that occurs when an invalid newtype is created that belongs to a data binding group  #1895 (@garyb)
- Fixed a case where re-exports included unintended exports #1872 (@garyb)
- Operator aliases can now be declared for qualified data constructors #2015 (@LiamGoodacre)
- A single `hiding` import will no longer raise an "unspecified imports" error #2017  (@garyb)
- Fixed a case where cycles in modules were being detected when they do not occur #2018 (@garyb)
- Various cases where files were not being read as UTF-8 on Windows were fixed #2027, #2031 (@garyb, @kRITZCREEK)
- Fixed some issues in pretty printing of records #2043 (@LiamGoodacre)
- `psci` now shows qualified imports correctly #2040 (@LiamGoodacre)
- Parser errors are now returned as JSON during IDE rebuild #2042 (@paf31)

## [v0.8.4](https://github.com/purescript/purescript/releases/tag/v0.8.4) - 2016-04-06

This is an interim bug fix release before 0.9.0.

**Enhancements**
- Check that FFI imports match with implementations (@hdgarrood)

  This is technically a breaking change, since some existing code might fail to compile if it has missing FFI code (`purescript-dom` is an example), but these libraries should be fixed soon.
- Import helper commands in psc-ide (@kRITZCREEK)

**Bug Fixes**
- Disallow constraint generalization for recursive functions. (#1978, @paf31)
- Fix #1991, instantiate polymorphic types before unification (@paf31)
- Use UTF8 when writing to stdout and stderr (@garyb)
- Fix for rendered constrained types needing parens. (@LiamGoodacre)
- everythingWithScope improperly traversing binary ops (@LiamGoodacre)

**Other**
- Update to use language-javascript 0.6.x (@nwolverson)

## [v0.8.3](https://github.com/purescript/purescript/releases/tag/v0.8.3) - 2016-03-26

**Breaking Changes**
- We have dropped support for GHC 7.8 and older (@hdgarrood)

**Enhancements**
- Infer types with class constraints (@paf31)

  For example, this simple code would previously have failed with a confusing `NoInstanceFound` error:

  ``` purescript
  add x y = x + y
  ```

  The compiler will now infer the most general type, namely `forall a. (Semiring a) => a -> a -> a`.

  Note that constraints can only be inferred if they only mention type variables; inference of arbitrary types in constraints is not (yet) supported. So, for example, you would still have to write a type signature for a function which had a constraint such as `(MonadEff (console :: CONSOLE | eff) m)`.
- Default require path to `../` (@nwolverson)

  The previous default behavior was no require path prefix, which was confusing for some workflows. The new default is `../`, which is the prefix used in `purs-loader`. This option will be removed completely in 0.9.
- Expose hiding import suggestion in JSON (@nwolverson)
- Error on missing `LICENSE` file or missing license field in `bower.json` (@faineance)

**Bug Fixes**
- Fix #1916 (@bagl)
- Fix detection of single open import (@garyb)
- Fix `true` not being treated as an infallible guard (@garyb)
- Fix pretty printer spinning (@garyb)
- Fix Windows build script (@garyb)
- Fix #1889, improve performance by avoiding whitespace operations on large strings (@paf31)

**psc-ide**
- Fix a crash related to error messages in the case splitting command (@kRITZCREEK)
- Escape regex characters when using the flex matcher (@kRITZCREEK)
- Adds `--help` commands to the `psc-ide` executables (@kRITZCREEK)
- Catches EOF exceptions thrown in `acceptCommand` (@kRITZCREEK)

**Other**
- Switched to Trusty distribution for Travis (@garyb)
- @kRITZCREEK and @faineance worked on refactoring the compiler.
- The `optparse-applicative` dependency was updated to `>= 0.12.1` (@stevejb71)
- The `bower-json` dependency was bumped (@hdgarrood)
- Better error message for `psc-publish` tests (@kRITZCREEK)
- Use generic Literal in the AST (@garyb)

## [v0.8.2](https://github.com/purescript/purescript/releases/tag/v0.8.2) - 2016-02-29

**Breaking Changes**

_None_

**Enhancements**
- `psc-ide` is now distributed with the compiler! (@kRITZCREEK)

  The `psc-ide-server` and `psc-ide-client` executables are now maintained and
  distributed alongside the compiler. This will ensure that the externs file
  format used by `psc-ide-server` is kept in sync with changes in the compiler.
- Source maps (@nwolverson)

  Source maps can be generated using the `--source-maps` flag. See the
  [example repository](https://github.com/nwolverson/purescript-sourcemap-test) for a full demonstration of source maps using Webpack.
- Operator aliases for data constructors (@garyb)

  Aliases can now be defined for data constructors. For example:

  ``` purescript
  data List a = Nil | Cons a (List a)

  infixr 6 Cons as :
  ```

  Here, the `:` operator can be used as a function to replace the `Cons` constructor,
  _and also in binders_.
- `Eq` and `Ord` deriving (@paf31)

  `Eq` and `Ord` instances can now be derived, using the `derive instance` syntax:

  ``` purescript
  derive instance eqList  :: (Eq  a) => Eq  (List a)
  derive instance ordList :: (Ord a) => Ord (List a)
  ```
- Types are now inferred in `psc-docs` and `psc-publish` (@hdgarrood)

  If type annotations are missing in source files, they will be inferred by
  `psc-docs` and `psc-publish` before documentation generation.
- Initial version of new syntax for operator sections (#1846, @paf31)

  Operator sections can now be written using underscores. For example:

  ``` purescript
  decrementAll :: Array Int -> Array Int
  decrementAll = map (_ - 1)
  ```

  which is equivalent to:

  ``` purescript
  decrementAll :: Array Int -> Array Int
  decrementAll = map (\x -> x - 1)
  ```

**Bug Fixes**
- Allow one open import without warning (@garyb)

  Warnings for open imports were a pain point for some users after the 0.8 release.
  This change allows a single open import without a warning. This is still safe
  in the presence of dependency updates, and does not lead to ambiguity for editor
  plugins searching for declaration sites.

**Other**
- @phadej has updated the Stack build to use the latest LTS and nightly builds.
- @izgzhen has refactored the PSCi code to be more readable.
- @hdgarrood has refactored the test suite.

## [v0.8.1](https://github.com/purescript/purescript/releases/tag/v0.8.1) - 2016-02-29

You are recommended to use v0.8.2 instead.

## [v0.8.0](https://github.com/purescript/purescript/releases/tag/v0.8.0) - 2016-01-31

A massive thanks to everyone involved in this release!

**Breaking Changes**

_None_, but there are lots of new warnings related to upcoming breaking changes in 0.9:
- Operators as aliases will become mandatory, and regular operators (as functions) will now generate warnings.
- Non-exhaustive functions will get a `Partial` constraint in 0.9, so the exhaustivity checker will now attempt to generate warnings by looking for `Partial` constraints in scope.
- The `qualified` import syntax has been deprecated.
- Class imports will use the new `class` syntax in 0.9 and the alternative syntax is deprecated.

**Enhancements**
- Add native `Partial` constraint (@garyb)
- Reduce backtracking in parser to hopefully improve quality of parsing error messages (@paf31)
- Drop requirement to parenthesize single constraints in instance contexts (@garyb)
- Case expressions can now match multiple values (@natefaubion)
- Add operator aliases (@garyb)
- Show identifiers correctly in ctags (@nwolverson)
- Fix #1523, add `--json-errors` flag for editor integrations (@paf31)
- Error and warning corrections are now available to editors via `--json-errors` (@nwolverson)
- Check integer values are within range in codegen (@garyb)
- Support for unicode operators (@paf31)
- The parser now supports unicode symbols for `forall` and function arrows (@DavidLindbom)
- Module Imports
  - Use `class` keyword for class references in imports (@garyb)
  - Type imports no longer require `()` (@garyb)
  - Allow import hiding with qualified imports (@garyb)
  - Naming conflicts are now resolved at the use site (@garyb)
- Error Messages
  - Fix #1662, display extra type info in instance errors (@paf31)
  - Add information about skolem constants to type errors (@paf31)
  - Sort rows in unification errors (@paf31)
- Warnings
  - Warn on unspecified imports (@garyb)
  - Warn when import X hiding (..) imports nothing (@garyb)
  - Warn on duplicate imports and exports (@garyb)
  - Warn about unused class imports (@garyb)

**Bug Fixes**
- Renamer updates, fixes naming bug in some unlikely situations (@garyb)
- Fix #1645, implement new indentation rules for types to avoid very wide errors (@paf31)
- Fix "resource exhausted" issue on MacOS (@mgmeier)
- Fix #1664, check kind before expanding wildcards. (@paf31)
- Fix up shadowed module names in JS codegen (@garyb)
- Fix #1185, fix #1369, add everythingWithScope traversal to correct some scoping issues. (@paf31)
- Fix two cases where errors were missing context (@garyb)
- Fix #1636, instantiate polytypes fully, even under constraints. (@paf31)
- Fix missing data constructors in re-exports (@garyb)
- Fix codegen error with instance for re-exported class (@garyb)
- Fix #1479, encode .js files as UTF8. (@paf31)
- Fix a bug related to redundancy checking in cases (#1853, @nicodelpiano)
- Fix a TCO/composition inlining bug (@garyb, @hdgarrood)
- Fix renaming for nested constructor binders (#1839, @sharkdp)
- Fix generic deriving bug with >1 type argument (@hdgarrood)
- Fix generate fresh binder names unless all names in case are equal (#1825, @paf31)
- Fix external require expressions when minifying (#1794, @paf31)
- Rename `foreign` argument to fix compiling issue (@anttih)
- Allow use of bottom integer (@garyb)

**Other**
- Fix #1700, remove warnings for syntactic features removed in 0.7.0 (@paf31)
- Fix psc-publish test (@passy)
- Relax rules for docs comments (#1820, @hdgarrood)
- Qualified name lookup is now supported in PSCi (#974, @soupi)
- https://github.com and git@github.com URLs are now allowed by psc-publish (@passy, @codedmart)
- Docs are now generated for module re-exports (@hdgarrood)
- Use friendly module name in psc-docs error (@nwolverson)
- Distinguish between the different ProperNames (@garyb)
- Warn about unspecified constructors in type imports (@garyb)
- Fix warning about values missing from virtual modules (@garyb)

## [v0.7.6.1](https://github.com/purescript/purescript/releases/tag/v0.7.6.1) - 2015-11-18

Fixes a bug in generic deriving.

See the [release notes for 0.7.6](https://github.com/purescript/purescript/releases/tag/v0.7.6).

## [v0.7.6](https://github.com/purescript/purescript/releases/tag/v0.7.6) - 2015-11-18

Thanks once again to everyone involved in this release!

This release includes some updates to generic deriving which require updating to the latest version of `purescript-generics`.

**Features**
- Field puns, fix #921 (@balajirrao)

  It is now possble to construct objects by using values in scope with the same name as the field labels. For example, the expression `{ foo, bar }` is equivalent to `{ foo: foo, bar: bar }`. Patterns desugar in the same way.

**Enhancements**
- Modules are now parsed in parallel (@paf31)
- Use `Types.Proxy.Proxy` instead of `Data.Generic.Proxy`. This fixes #1573 (@tfausak)
- Update generic deriving for latest `purescript-generics` changes (@paf31)
- New import warnings - unused data constructors, unused imports (@nwolverson)
- `psc-publish`: only warn on dirty working tree on dry runs (@hdgarrood)
- Add more information to psci :browse command (@soupi)
- Add support for --require-path option to psc-bundle (@natefaubion)
- Improved error reporting in psc-publish (@hdgarrood)
- Reduce noise in instance declarations in documentation (@hdgarrood)

**Bug Fixes**
- New approach to unification, fixing some loops in the type checker (@paf31)
- Fix #1632, instantiate type variables in anyProxy calls in generic instances (@paf31)
- Fix warnings for unqualified implicit imports (@nwolverson)
- Fix #1596, don't show type checker warnings in the event of an error (@paf31)
- Fix #1602, improvements around code generation of string literals (@michaelficarra)
- Fix #1090, allow accessors in operator sections (@paf31)
- Fix #1590, limit depth of pretty-printed expressions (@paf31)
- Fix #1591, use the 'negate' in scope (@paf31)
- Fix #1335, track scoped type variables when skolemizing (@paf31)
- Fix #1175, check types inside where clauses inside instances (@paf31)
- Some refactoring (@phadej)
- Fixed some error messages (@zudov)

**Deployment**
- Use `base-compat` to reduce the need for `CPP` (@phadej)
- Write license-generator in Haskell (@phadej)
- Add GHC 7.10.3 to CI build matrix (@phadej)

## [v0.7.5.3](https://github.com/purescript/purescript/releases/tag/v0.7.5.3) - 2015-10-29

**Bug Fixes**
- #1072, #1130, #1578, #1577, #1582

## [v0.7.5.2](https://github.com/purescript/purescript/releases/tag/v0.7.5.2) - 2015-10-27

Fixes a build issue with GHC versions < 7.10. Functionally equivalent to v0.7.5.1.

## [v0.7.5.1](https://github.com/purescript/purescript/releases/tag/v0.7.5.1) - 2015-10-27

**Bug Fixes**
- Fix #1169, #1315, #1534, #1543, #1548, #1551, #1557, #1570
- Fix memory leak caused by WriterT (#1297) by @paf31
- Display hints after main error (#1563) by @paf31
- Friendlier errors by @paf31
- Documentation fixes by @nwolverson
- Haddock fixes by @trofi

## [v0.7.5](https://github.com/purescript/purescript/releases/tag/v0.7.5) - 2015-10-20

A big thank you to everyone who was involved in this release, from filing issues, through fixing bugs to testing patches.

The main focus areas for this release, as part of the 0.8 milestone, were error messages and performance.

**Breaking Changes**

_None!_

**Enhancements**
- Pretty printing of types and expressions in errors was improved (@paf31)
- Externs files are now saved as JSON (@paf31)
- Support for parallel builds has been added (@paf31)
  Builds will now use multiple cores by default, but the number of capabilities can be modified by passing the `-N` option to the GHC runtime:

  ``` text
  psc <input files> +RTS -N8
  ```
- Binders can now be given type annotations (@5outh)

  For example:

  ``` purescript
  example = do
    n :: Int <- get
    put (n + 1)
  ```

  This can be useful when disambiguating types.
- There is a new warning for missing type signatures on top-level declarations (@paf31)
- There are new warnings for shadowed and unused type variables (@garyb)
- Contextual information in warnings was improved (@garyb)
- The `qualified` keyword is now optional when importing modules qualified (@michaelficarra)
- @zudov changed the behavior of PSCi on CTRL+C/D to match GHCi and other REPLs.
- A bug in row unification was fixed (#1310, @paf31)
- Constrained types can now be defined without a `forall` keyword. This is useful in some nullary type class and rank-N scenarios. (@paf31)

**Bug Fixes**
- @garyb added some additional checks for transitive module exports.
- Type synonyms are now expanded more eagerly to avoid some error cases in the type checker (@paf31)
- Better support for multi-byte UTF-8 characters (@senju)
- A check has been added to the exhaustivity checker to avoid exponential blowup (@paf31)
- Empty case statements are no longer syntactically valid (@zudov)

**Other**
- @aspidites fixed all compiler warnings in the core libraries.
- @zudov and @phadej have made improvements to the Stack distribution of the compiler, and the Stackage builds.
- @garyb has added a warning for operators in type classes, since they will be disallowed before 0.8.

## [v0.7.4.1](https://github.com/purescript/purescript/releases/tag/v0.7.4.1) - 2015-08-26

This patch release fixes two bugs related to the new instance resolution algorithm and overlapping instances:
- `psci` would not work due to overlaps in the `PSCI.Support` module
- `free` would not build due to its dependency on `inject`

The solution for now is to make overlapping instances into a _warning_ (instead of an error) at the site of their use.

Later we might revisit this decision and allow the user to express classes like `Inject` which are necessarily overlapping.

## [v0.7.4.0](https://github.com/purescript/purescript/releases/tag/v0.7.4.0) - 2015-08-25

**Breaking Changes**
- The type class instance search algorithm has changed. The search will now eagerly match an instance for each subgoal based on the instance head, or fail. This makes certain instances in previous versions of `purescript-transformers` invalid, so users of this release should upgrade to the latest `transformers`.
- A module must be imported to be re-exported.

**Enhancements**
- `RedefinedModule` errors now include position info #1024 (@garyb)
- Multiple imports of the same module are now resolved correctly, allowing for combinations of qualified and unqualified importing #817 #1112 (@garyb)
- Errors for unresolvable imports and exports have been clarified #1232 (@garyb)
- A warning is emitted when importing `Type(..)` when `Type` is a synonym or has no constructors. #1391 (@garyb)
- Superclass constraints can now be relied upon when resolving instances #421 (@paf31)
- A serious performance regression was partially addressed, memory usage should now be drastically reduced #1297 (@garyb)
- Module re-export handling has been much improved. If a module is partially imported, only the specifically imported members are re-exported. Qualified modules can also be re-exported. #291 #1244 (@garyb)
- Parser error messages are now formatted in a manner more consistent with other errors #1098 (@epost)
- Using `-ffi` to specify JavaScript FFI files is now optional, files with a `.js` extension will be detected as FFI files automatically when encountered. #1268 (@mjgpy3)

**Bug fixes**
- Fixed an error when attempting to derive for `Void` #1380 (@nicodelpiano)
- `"The impossible happened in desugarDo"` should no longer occur #386 (@paf31)

**Other**

@zudov, @phadej and @erdeszt made more updates and improvements to the CI build.

## [v0.7.3](https://github.com/purescript/purescript/releases/tag/v0.7.3) - 2015-08-13

**Major Features**
- @gbaz has implemented **generic deriving**. This allows instances for the `Generic` class in the `purescript-generics` package to be derived by the compiler.

  A `Generic` instance can be derived as follows:

  ``` purescript
  data Example = Foo String | Bar Int | Baz Boolean

  derive instance genericExample :: Generic Example
  ```

  `purescript-generics` provides examples of usage, such as `gShow`, `gEq` and `gCompare`, for printing, equality tests and comparison respectively.

  See #1138.
- @garyb has implemented a test for **orphan instances** which will now cause the build to fail with an error. See #1247

**Enhancements**
- @mjgpy3 has added a warning when an input glob does not match any files.

**Bug Fixes**
- The `psc: <<loop>>` has been fixed. This was due to a bug in the error pretty printer. (@paf31)
- An issue with unicode characters in string literals was fixed by @michaelficarra.
- Compiler errors are now pretty printed in `psc-publish` (@paf31)
- Modules are no longer linted if they are not being rebuilt (@paf31)
- FFI bindings are now reloaded when changed, in PSCi (@paf31)

**Other**
- @phadej and @zudov have improved our CI process, so that PureScript now compiles against three versions of GHC and two LTS Stackage releases, as well as the nightly stackage releases.
- @phadej and @lukerandall have worked on supporting PureScript in Stackage.

## [v0.7.2.1](https://github.com/purescript/purescript/releases/tag/v0.7.2.1) - 2015-08-12

Functionally equivalent to v0.7.2. This release fixes a version incompatibility with Stackage.

## [v0.7.2](https://github.com/purescript/purescript/releases/tag/v0.7.2) - 2015-08-03

**Bug fixes**
- Fixed haddock for the Language.PureScript.Bundle module #1262 (@wuzzeb)
- Some erroneous error positions were fixed for kind and missing instance errors #1086 (@garyb)
- The number of warnings printed for exhaustivity checks was limited to 5 #1281 (@nicodelpiano)
- Home directory is checked for `.psci` file _after_ the current working directory #883 (@mjgpy3)
- Line numbers now show for shadowed name warnings #1165 (@nicodelpiano)
- Cabal file was fixed for Nix packaging #1302 (@MasseGuillaume)
- Kind query for types defined in psci now works #1235 (@mjgpy3)
- Boolean operations are now being inlined again #1312 (@garyb)
- Int operations are now being inlined again #1330 (@garyb)
- "Writing..." and "Compiling..." messages are no-longer printed in `psci` #1276 (@paf31)

**Enhancements**
- Exhaustivity checker was extended to report errors about redundant cases #1289 (@nicodelpiano)
- Improved triggering of suggestion for errors about using `(<<<)` instead of `(.)` #1284 (@mjgpy3)
- Only show the module name rather than the filename for pattern errors #1296 (@nicodelpiano)
- Error reporting in `psc-bundle` was improved #1307 (@hdgarrood)
- `psc-publish` code is now part of the library module #1304 (@hdgarrood)
- `psc-publish` now has `--version` and `--help` options #1300 (@garyb)
- `psc-publish` now has a `--dry-run` option for checking whether the module can be published #1308 (@hdgarrood)
- `psc-publish` now requires a clean working tree #1306 (@hdgarrood)
- `psc-publish` can now find `bower` on Windows machines #1317 (@hdgarrood)
- `psc-publish` now uses OS-specific path delimiters to fix another bug on Windows #1326 (@hdgarrood)
- Error list heading was made emacs-friendly #1327 (@epost)

## [v0.7.1](https://github.com/purescript/purescript/releases/tag/v0.7.1) - 2015-07-13

Minor fixes after 0.7.0:
- @hdgarrood has worked on improvements to `psc-publish` to support the new Pursuit website.
- @mjgpy3 has improved warning messages
- @wuzzeb has improved the pretty printers
- @hdgarrood has added CI builds for GHC 7.10 and 7.6

Enhancements
- @nicodelpiano has added exhaustivity checking as a new warning type. Incomplete pattern matches will now generate warnings like this:

  ``` text
  Warning in module Data.Either.Unsafe:
    Warning in value declaration fromRight:
    Warning at src/Data/Either/Unsafe.purs line 14, column 1 - line 15, column 1:
  Pattern could not be determined to cover all cases.
      The definition has the following uncovered cases:
        (Data.Either.Left _)
    See https://github.com/purescript/purescript/wiki/Error-Code-NotExhaustivePattern for more information, or to contribute content related to this error.
  ```

## [v0.7.0](https://github.com/purescript/purescript/releases/tag/v0.7.0) - 2015-06-30

**Introduction**

This release ("MELTDOWN") aims to handle as many planned breaking changes as possible, to ease the upgrade path before 1.0. It is necessary to upgrade almost all PureScript code to compile with this release.

The latest versions of the core libraries have all been updated to compile with this release. Older versions of the core libraries will not work with this release, and the latest versions of libraries will not build with older compiler releases.

Detailed instructions for those who need to migrate their code can be found [on the wiki](https://github.com/purescript/purescript/wiki/0.7-Migration-Guide).

As usual, many thanks go to all of the contributors who helped with this release!

**Breaking changes**
- The `psc` executable has been replaced with `psc-make`, which has been renamed to `psc` (in an effort to standardize on CommonJS module output). Features which were previously only available in old `psc` (dead code elimination, bundling code for the browser) are now handled by the new executable `psc-bundle`, which works with the output of the new `psc` (for faster, incremental builds).
- There are now `Int` and `Number` literals. To disambiguate the two, integer `Number` values must now be written with a decimal place (`3.0` rather than `3`).
- The `Prelude` module is no longer imported automatically, and must be imported the same way as any other module.
- No modules are included with the compiler now, they have been broken out into their own libraries:
  - [purescript-prelude](https://github.com/purescript/purescript-prelude)
  - [purescript-eff](https://github.com/purescript/purescript-eff)
  - [purescript-st](https://github.com/purescript/purescript-st)
  - [purescript-console](https://github.com/purescript/purescript-console)
  - [purescript-functions](https://github.com/purescript/purescript-functions)
- `Debug.Trace` has been renamed to `Control.Monad.Eff.Console`, and `trace` has been renamed to `log`.
- `[]` syntax for array types has been removed. It is still possible to use `[]` array literals however.
  - `[]` should now be written as `Array`, and `[a]` as `Array a`.
- Cons patterns for arrays have been removed.
- Declaring operators in classes will now produce a warning. Changes will be coming to operators in PureScript 0.8, and moving to named members in classes with operators as aliases (e.g. `(<$>) = map`) should make the transition easier in the future.
- JavaScript for the FFI can no longer be provided inline.
  - Values must instead be provided in a separate `.js` file, and passed to the compiler with the `-ffi` flag.
  - Values should be provided in the form `exports.foo = ...`, similar to a CommonJS module
  - The file should have a comment `// module X.Y.Z` where `X.Y.Z` is the name of the module the JS values are for.
  - [See here for an example](https://github.com/purescript/purescript-eff/blob/v0.1.0-rc.1/src/Control/Monad/Eff.js)

**Enhancements**
- Module exports (@andyarvanitis). Currently, only full module exports are supported, but imported modules can be re-exported using the following syntax:
  `purescript
  module M1 (module M2) where
  import M2
  `
- Documentation improvements (@hdgarrood):
  - `psc-docs` can now generate multiple output files, allowing documentation to be collected into functional groups.
  - A new tool `psc-publish` has been added, which generates module documentation in a JSON format required by Pursuit 2 (coming soon)
- @hdgarrood has worked on improving the quality of code completion inside `psci`, and generally tidying up and refactoring that code.
- @puffnfresh has worked on dramatically increasing the performance of incremental builds, with improvements up to 10x compared to the previous release.
- The new `--require-path` option allows the syntax of module imports in generated CommonJS modules to be customized (@garyb).
- @etrepum has added support for building with Stack.
- PSCi now supports computations in the `Eff` monad. (@paf31)
- The compiler now emits warnings in the following cases:
  - Operator name used in type class definition (@garyb)
  - Type wildcard used (@nicodelpiano)
  - Shadowed variable name (@paf31)
- @balajirrao has improved the appearance of unknown and rigid types appearing in error messages.
- @puffnfresh has added position information to pattern match errors.
- @puffnfresh has added some new optimizations (inlining `<<<` and `$`)

**Bug Fixes**
- `psc`, `psc-docs` and `psc-bundle` now support file globs as command-line arguments, fixing a bug related to the command length on Windows machines (@paf31)
- @wuzzeb has fixed some issues in the pretty printer.
- @mjgpy3 has improved error messages related to incorrect pattern matches on data constructors.

**Tools**
- Pulp has been updated:
  - The new `psc` and `psc-bundle` binaries are supported
  - FFI modules are now identified and compiled based on a convention
  - `pulp docs` now generates individual Markdown files for each source module
- `gulp-purescript` has been updated:
  - The new `psc` and `psc-bundle` binaries are supported
  - FFI modules are now supported

**Libraries**
- The following libraries have been moved into the core library set:
  - `purescript-lists` - Strict and lazy linked list data structures
  - `purescript-assert` - Low level assertion library for tests
  - `purescript-parallel` - An applicative functor for parallel composition of asynchronous computations.
  - `purescript-arrows` - Arrow type classes and standard instances.
  - `purescript-tailrec` - A type class for stack-safe monadic tail recursion.
- The requirements for libraries in the `purescript-contrib` organization [have been tightened](https://github.com/purescript/purescript/wiki/Contrib-Guidelines), to try to ensure that libraries stay maintained.

## [v0.7.0-rc.1](https://github.com/purescript/purescript/releases/tag/v0.7.0-rc.1) - 2015-06-07

**Important note**

This release should be used with the latest versions of the core libraries, which are also tagged as `-rc.1`.

**Breaking changes**
- There are now `Int` and `Number` literals. To disambiguate the two, integer `Number` values must now be written with a decimal place (`3.0` rather than `3`).
- The `Prelude` module is no longer imported automatically, and must be imported the same way as any other module.
- No modules are included with the compiler now, they have been broken out into their own libraries:
  - [purescript-prelude](https://github.com/purescript/purescript-prelude)
  - [purescript-eff](https://github.com/purescript/purescript-eff)
  - [purescript-st](https://github.com/purescript/purescript-st)
  - [purescript-console](https://github.com/purescript/purescript-console)
  - [purescript-functions](https://github.com/purescript/purescript-functions)
- `[]` syntax for array types has been removed. It is still possible to use `[]` array literals however.
  - `[]` should now be written as `Array`, and `[a]` as `Array a`.
- Cons patterns for arrays have been removed.
- Declaring operators in classes will now produce a warning. Changes will be coming to operators in PureScript 0.8, and moving to named members in classes with operators as aliases (e.g. `(<$>) = map`) should make the transition easier in the future.
- JavaScript for the FFI can no longer be provided inline.
  - Values must instead be provided in a separate `.js` file, and passed to the compiler with the `-ffi` flag.
  - Values should be provided in the form `exports.foo = ...`, similar to a CommonJS module
  - The file should have a coment `// module X.Y.Z` where `X.Y.Z` is the name of the module the JS values are for.
  - [See here for an example](https://github.com/purescript/purescript-eff/blob/v0.1.0-rc.1/src/Control/Monad/Eff.js)

_Full release notes coming soon_

## [v0.6.9.5](https://github.com/purescript/purescript/releases/tag/v0.6.9.5) - 2015-04-25

This release contains two patches:
- Case statements were generating incorrect function name arguments #1008 (@paf31)
- Comments and verbose error flags were mixed up #991 (@garyb)

## [v0.6.9.3](https://github.com/purescript/purescript/releases/tag/v0.6.9.3) - 2015-03-18

**Breaking Changes**
- `refEq` and `refIneq` are no longer exported from the `Prelude`.

**Bug Fixes**
- Instances can now be defined before the corresponding class declaration (@paf31)
- A bug related to imports in `psci` was fixed. (@paf31)
- A typechecker bug related to type class dictionaries was fixed. (@garyb)
- A bug related to operator precedence in codegen was fixed. (@garyb)

**Enhancements**
- `psci` now supports long-form directives (@mrhania)
- Syntax for imports and other declaration types in `psci` was improved. (@hdgarrood)
- Markdown comments can now be included at the module level (@joneshf)
- Error messages are now represented internally as an algebraic data type, and pretty printing has been improved by using the `boxes` library. Errors now link to the wiki. (@paf31)
- `psc-docs` can now generate tags files for Vim and Emacs (@jacereda)
- `psci` now supports a `--node-opts` flag for passing options to the Node executable. (@MichaelXavier)
- Code gen now preserves names of more function arguments in case statements (@andyarvanitis)
- There is now a `Semigroup` instance for `Ordering` (@pseudonom)

**Documentation**
- The Prelude now has Markdown documentation (various contributors - thank you!)
- The [Pursuit](http://pursuit.purescript.org) website has been updated with new versions of libraries, including Markdown documentation (@hdgarrood)

**Libraries**
- The following libraries are now core libraries:
  - `purescript-tailrec` - A type class for monadic tail recursion
  - `purescript-monad-eff` - A type class for monads supporting native effects
  - `purescript-integers` - Integer numeric type
  - `purescript-invariant` - Invariant functors
  - `purescript-parallel` - An applicative functor for parallel composition of asynchronous computations

**Other**
- There is an experimental C++11 backend for PureScript called [pure11](https://github.com/andyarvanitis/pure11).

## [v0.6.8](https://github.com/purescript/purescript/releases/tag/v0.6.8) - 2015-02-21

**Breaking Changes**
- The `Num` type class has been refined to allow more interesting instances. The `Semiring`, `ModuloSemiring`, `Ring` and `DivisionRing` classes have been introduced. Most code should continue to compile, since `Number` was one of only a handful of instances, but library developers will need to break up their `Num` instances.

**Enhancements**
- @garyb has improved the readability of `psc-docs` output.

**Notes**
- All uses of the deprecated `ErrorT` have been replaced with `ExceptT` and the `transformers` and `mtl` dependencies bumped accordingly.

## [v0.6.7.1](https://github.com/purescript/purescript/releases/tag/v0.6.7.1) - 2015-02-14

**Bug Fixes**
- A fix for a bug in the type class instance resolution code (#870, @paf31)

## [v0.6.7](https://github.com/purescript/purescript/releases/tag/v0.6.7) - 2015-02-12

**Enhancements**

**Scoped Type Variables**

(#347, @paf31)

This feature allows type variables which are bound by a `forall` keyword to be used inside type annotations in the body of the function. For example, suppose we want to define a `map` function on a `List` type:

``` purescript
data List a = Nil | Cons a (List a)

map :: forall a b. (a -> b) -> List a -> List b
map f = go
  where
  go Nil = Nil
  go (Cons x xs) = Cons (f x) (map f xs)
```

To give a type to `go`, we could previously use type wildcards:

``` purescript
go :: List _ -> List _
```

Now, we can refer to the types `a` and `b` inside the type of `go`, giving a more precise type:

``` purescript
go :: List a -> List b
```

**Rows In Instance Contexts**

(@paf31, @apsk)

This feature allows rows to appear on the left of a `=>` in a type signature. For example, given a `MonadEff` class:

``` purescript
class MonadEff eff m where
  liftEff :: forall a. Eff eff a -> m a
```

we can now write the following function which works in any `Monad` supporting `Trace` actions:

``` purescript
logging :: forall m a eff. (Monad m, MonadEff (trace :: Trace | eff) m) => String -> m a -> m a
logging s action = do
  liftEff $ trace $ "Starting: " <> s
  a <- action
  liftEff $ trace $ "Done: " <> s
  return a
```

**Improved `let` bindings in `psci`**

(#782, @paf31)

Any declaration can now be used inside a `let` binding in `psci`. For example, we can define data types or foreign imports:

``` text
> let data Foo = Foo | Bar | Baz

> let foreign import foo :: Foo -> String
```

The general form of a `let` statement in `psci` now contains one or more declarations of any type, and these declarations simply get added to the current module.

As a bonus, polymorphic functions bound using `let` now work at multiple type instantiations in `psci`:

``` text
> let f x = x

> if f true then f "true" else f "False"
"true"
```

**Markdown Support in `psc-docs`**

(#802, @paf31)

Markdown can now be used for documentation purposes by using pipe characters to align content. For example:

``` purescript
-- | Create a copy of the array without its first element.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
-- |
-- | This function is partial. Specifically, `tail []` is undefined.
tail :: forall a. [a] -> [a]
```

`psc-docs` will insert this markdown content verbatim into your generated documentation.

**Bug Fixes**
- Modules are rebuilt before a command is executed in `psci`, to avoid situations where compiled code becomes out-of-date (@paf31)
- `@` is a valid operator name again (#815, @paf31)
- Reserved module names are now properly escaped (@garyb)

## [v0.6.6](https://github.com/purescript/purescript/releases/tag/v0.6.6) - 2015-02-09

**Breaking Changes**
- The syntax of record getters was changed to `_.prop` (@garyb)

**Enhancements**
- The record part of a record updater can now be made into a wildcard, e.g. `_ { foo = 1 }` (@garyb)
- Extended infix expressions are now supported, (@paf31) e.g.

  ```
  [1, 2, 3] `zipWith (+)` [4, 5, 6]
  ```

**Bug Fixes**
- Newline issues were fixed in executables (@michaelficarra)

## [v0.6.5](https://github.com/purescript/purescript/releases/tag/v0.6.5) - 2015-02-08

**Enhancements**
- Lightweight record constructors are now supported (@garyb):

  ``` purescript
  person :: Maybe String -> Maybe Number -> Maybe Address -> Maybe Person
  person = { name: _, age: _, location: _ } <$> name <*> age <*> location
  ```
- Field accessor sections are now supported (@garyb):

  ``` purescript
  getPersonName :: Maybe String
  getPersonName = (.name) <$> getPersonInfo
  ```
- Syntactic sugar has been introduced for object update functions:

  ``` purescript
  updateName :: Person -> String -> Person
  updateName person = person { name = _ }
  ```
- Operator sections are now supported (@garyb)

**Bug Fixes**
- Some command line options were fixed in `psc-make` (@paulyoung)
- Some module import errors were fixed (@garyb)
- A typechecker bug related to row synonyms was fixed (#795, @paf31)

## [v0.6.4.1](https://github.com/purescript/purescript/releases/tag/v0.6.4.1) - 2015-02-03

Various small bug fixes.

## [v0.6.4](https://github.com/purescript/purescript/releases/tag/v0.6.4) - 2015-01-23

- Fix some precedence issues in the code generator.
- Tighten the bounds on `utf8-string`.
- Fixed a bug in the typechecker.

## [v0.6.3](https://github.com/purescript/purescript/releases/tag/v0.6.3) - 2015-01-08

**Breaking Changes**

**Bug Fixes**
- Case statement at end of `Eff` block not being executed. (#759, @paf31)
- A bug related to dead code elimination was fixed. (@garyb)
- Wildcards can now appear in row endings. (@RossMeikleham)

**Enhancements**
- There is a new "core functional representation", which will enable certain optimizations, and new features such as rewrite rules. (#710, @garyb)
- Record pattern matches now allow field names to be separated from binders using `:` instead of `=`, to match record construction (#760, @leighman)
- Some improvements needed for the Pursuit tool (@hdgarrood)
- The lexer was separated from the parser, and now supports explicit comments in the AST. Documentation generated by `psc-docs` now contains any inline comments which precede the corresponding declaration, and generated code preserves the same comments. (@paf31)
- PureScript now builds on GHC 7.6.\* again. (@dylex)
- Proper names can now contain underscores. (@dylex)
- Several auto-completion improvements and fixes in PSCI. (@vkorablin)

**Libraries**
- The Prelude now contains a `pureST` function to run `ST` computations in a pure context. (@KMahoney)

**Tools**
- The Pursuit tool now runs on the community server, and integrates with Bower. Libraries can be added by submitting a pull request. (@hdgarrood)

## [v0.6.2](https://github.com/purescript/purescript/releases/tag/v0.6.2) - 2014-11-28

**Breaking Changes**
- Command line options with multiplicity 1 now require an equals symbol, e.g.

  ```
  psc --main=Main --browser-namespace=PS
  ```

  The Grunt and Gulp plugins already support this format.

**Enhancements**
- Use `optparse-applicative` instead of `cmdtheline` (@anthoq88)

**Libraries**
- Move `STArray` out of Prelude. (@paf31)

## [v0.6.1.2](https://github.com/purescript/purescript/releases/tag/v0.6.1.2) - 2014-11-24



## [v0.6.1.1](https://github.com/purescript/purescript/releases/tag/v0.6.1.1) - 2014-11-19

**Breaking Changes**
- The pipe symbol is now a reserved operator.
- The operators in the `Bits` type class have been renamed.

**Enhancements**
- Fix build on GHC 7.6.\* (@dylex)
- Relax indentation requirements (@paf31)

## [v0.6.1](https://github.com/purescript/purescript/releases/tag/v0.6.1) - 2014-11-18

**Breaking Changes**
- The body of a guarded expression must now be indented _past the guard_. For example, this is valid:

``` haskell
positive n | n > 0 = true
positive _ = false
```

but this is not:

``` haskell
positive n | n > 0
  = true
positive _ = false
```

**New Features**
- Type wildcards are now supported (#287, @paf31)

**Enhancements**
- Allow unquoted keywords as key names in record literals (#606, @michaelficarra)
- Import instances when referencing qualified values (#667, @garyb)
- Multiple guard clauses are now supported (#294, @paf31)
- Type check let declarations immediately in `psci` (#615, @garyb)

## [v0.6.0.2](https://github.com/purescript/purescript/releases/tag/v0.6.0.2) - 2014-11-09

- Prevent `psci` and `psc-make` from rebuilding everything on every build #692

## [v0.6.0](https://github.com/purescript/purescript/releases/tag/v0.6.0) - 2014-11-06

For more information on PureScript, see the [purescript.org website](http://purescript.org).

**Breaking Changes**
- The `Alternative` type class hierarchy was refactored. See [here](https://github.com/purescript/purescript-control/issues/6).
- `--runtime-type-checks` has been removed. The recommended approach is to use `purescript-foreign`. (@garyb)
- The `Unit` type is now used in the Prelude and core libraries to represent values containing no data. (@garyb)
- The Prelude is no longer distributed as a separate file, but is embedded in the compiler executables. (@paf31)
- `docgen` is now called `psc-docs`.

**New Features**
- Newtypes are now supported using the `newtype` keyword. The runtime representation of a newtype is identical to that of the contained type. (@garyb)
- Multiline string literals are now supported via triple-quote syntax, making FFI declarations much neater. (@phadej)
- Kind signatures on types and type constructor arguments are now supported. (@paf31)

**Enhancements**
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

**Bug Fixes**
- Binding group errors in type class members are now caught at compile time. (@dylex)
- Some errors related to type checking rows with duplicate labels were fixed. (@paf31)
- Some issues with the calculation of binding groups were fixed. (@paf31)
- Error messages for invalid case declarations are now generated. (@natefaubion)
- Some issues related to module exports were fixed. (@garyb)
- `psci` now checks imports for validity. (@Bogdanp)

**Libraries**
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

**Documentation**
- The [PureScript book](https://leanpub.com/purescript/read) is now available.
- The [PureScript wiki](https://github.com/purescript/purescript/wiki) is now the main resource for compiler and library documentation.

## [v0.5.7.1](https://github.com/purescript/purescript/releases/tag/v0.5.7.1) - 2014-10-30



## [v0.5.7](https://github.com/purescript/purescript/releases/tag/v0.5.7) - 2014-10-29



## [0.5.6.1](https://github.com/purescript/purescript/releases/tag/0.5.6.1) - 2014-10-06



## [0.5.6](https://github.com/purescript/purescript/releases/tag/0.5.6) - 2014-10-06



## [v0.5.6.3](https://github.com/purescript/purescript/releases/tag/v0.5.6.3) - 2014-10-06



## [0.5.6.2](https://github.com/purescript/purescript/releases/tag/0.5.6.2) - 2014-09-22



## [v0.5.5](https://github.com/purescript/purescript/releases/tag/v0.5.5) - 2014-09-02



## [v0.5.4](https://github.com/purescript/purescript/releases/tag/v0.5.4) - 2014-08-04

This incremental release is provided to provide bug fixes and features required to compile the latest core libraries.

## [v0.5.0](https://github.com/purescript/purescript/releases/tag/v0.5.0) - 2014-04-27

**Breaking Changes**
- Support for blocks has been removed. (paf31)
- Type class instances must now be named (paf31)

  ```
  instance showNumber :: Show Number where
    ...
  ```
- Prelude modules now follow a naming scheme similar to haskell (e.g. `Data.Maybe`, `Control.Monad`) (garyb)
- Many modules that were previously part of the Prelude have been split into individual libraries, [now distributed via Bower](http://bower.io/search/?q=purescript) (garyb)
- Multiple modules with the same name are now disallowed rather than merged (garyb)
- The `Prelude` module is now imported automatically. Conflicts can be avoided by using qualified imports or an explicit import list. (garyb, paf31)
- Overlapping instances are no longer allowed. The `Prelude` and core libraries have been updated accordingly. (paf31)
- `Functor`, `Applicative`, `Monad` are now part of a class heirarchy that include `Apply` and `Bind`. `return` is now an alias for `pure`. (joneshf, paf31, garyb)
- `Semigroupoid` is now a superclass of `Category` (garyb)
- `(:)` is now part of Prelude (garyb)
- `(!!)` has been renamed to `Prelude.Unsafe.unsafeIndex` and a safe version has been added to `Data.Array` (garyb)

**New Features**
- Multi parameter typeclasses (paf31)
- Superclasses (puffnfresh, paf31)
- FlexibleInstances and FlexibleContexts (paf31)
- Let bindings are now supported. The `let` keyword can introduce several local (possibly mutually recursive) bindings, along with optional type signatures. (paf31)
- `where` clauses are now supported in value declarations, with the same rules as `let` bindings (garyb)
- Empty data declarations and empty type classes are now supported (paf31)
- A new command line option `--codegen` controls which modules will have Javascript and externs generated (paf31)
- `psc-make` now generates CommonJS-compatible modules, which can be used with `require()` in `node`. `psc` still generates modules for use in the browser. (paf31, garyb)

**Enhancements**
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
**The Object type constructor can now be referenced explicitly as `Prim.Object` (with kind `# * -> *`) (paf31)**
- Optimizations are now enabled by default and can be disabled with the `--no-tco` and `--no-magic-do` flags (garyb)
- Unary minus and signed numeric literals are now supported again (paf31, garyb)
- Type errors have been simplified, the full trace can be enabled with `--verbose-errors` or `-v` (paf31)
- Error messages now display source positions (paf31, garyb)
- The type classes implementation and code generation was greatly simplified (paf31)
- Object properties and row labels can now be accessed with arbitrary string names by using string literals (paf31)
- `(++)` is now an alias for the Semigroup operator `(<>)` (paf31)
- Error messages for classes with undefined or missing members have been improved (garyb)
- The SYB dependency was removed, and traversals rewritten by hand, for a large performance increase (paf31)

**Bug Fixes**
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

**Libraries**
- Purescript libraries are now [distributed via Bower](http://bower.io/search/?q=purescript). There are currently around 40 libraries available.

**Plugins**
- The `grunt-purescript` plugin has been updated to provide support for new command line options.
- There is a new `gulp-purescript` plugin available for compiling with Gulp.

**Documentation**
- There is a new `hierarchy` executable which will generate `.dot` diagrams based on the type class hierarchy of a module. The Prelude docs have been updated to include such a type class diagram. (joneshf)
