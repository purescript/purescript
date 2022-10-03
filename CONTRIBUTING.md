# Contributing to the PureScript Compiler

## Reporting Issues

When reporting issues, please be aware of the following:

* Please use the appropriate issue template if there is one: filling out all of the sections in the template makes it much easier for us to understand what the problem is and how we might want to address it.
* We prefer to reserve the issue tracker in this repository for tasks which involve work on the compiler. If your report or proposal doesn't involve work on the compiler, please open it on the repository where the work would be done. If you're unsure, you can always ask on [Discord](https://purescript.org/chat) or [Discourse](https://discourse.purescript.org).
* If you have a question or need help, please ask on [Discord](https://purescript.org/chat) or [Discourse](https://discourse.purescript.org) instead.
* When submitting feature proposals, please be aware that we prefer to be conservative about adding things to the language/compiler. A feature proposal is much more likely to be accepted if it includes a clear description of the problem it intends to solve, as well as not only a strong justification for why adding the feature will solve that problem, but also for why any existing features or techniques that could be used to solve that problem are insufficient.

We have defined some [Project Values](https://github.com/purescript/governance#project-values) in our organization's governance document; referring to these may help you get a better idea of what is likely to be accepted and what isn't.

## Sending Pull Requests

Pull requests are encouraged, but please open issues before starting to work on something that you intend to make into a PR, so that we can decide if it is a good fit or not.

### Finding Issues to Work On

If you would like to contribute, please consider the issues in the current milestone first. If you are a new contributor, you may want to have a go at the ["new contributor" issues](https://github.com/purescript/purescript/labels/new%20contributor) to get started.

### Submitting Your Code

When submitting a pull request, please follow the following guidelines:

- Add tests according to the next section
- Build the binaries and libraries with `stack build --fast`. The `--fast` flag is recommended but not required; it disables optimizations, which can speed things up quite a bit.
- Make sure that all test suites are passing. Run the test suites with `stack test --fast`.
- Please try to keep changes small and isolated: smaller pull requests which only address one issue are much easier to review.
- For any code change, please append a copyright and licensing notice to the [CONTRIBUTORS.md](CONTRIBUTORS.md) file if your name is not in there already.

### Writing Tests

When writing tests, try to have at least one passing test and one failing test, if applicable.

- Passing tests go in `tests/purs/passing/`
- Failing tests go in `tests/purs/failing/`
- Tests that check warnings go in `tests/purs/warning/`

Passing tests may produce warnings. Tests in `tests/purs/warning/` can ensure no warning is emitted by having no annotations and an empty `.out` file. 

### Running Tests

Run all test suites with `stack test`. You will need `npm`, `bower` and `node` on your PATH to run the tests.

You can run individual test suites using `stack test --test-arguments="--match PATTERN"` where `PATTERN` is one of `compiler`, `repl`, `ide`, `docs`, `corefn`, or `hierarchy`. You can also build and run a specific test in `tests/purs/passing/` or `tests/purs/failing/` by using the test's filename as the pattern, e.g.:

```
stack test --fast --test-arguments="--match 1110.purs"
```

This will run whatever test uses the example file `1110.purs`.

The golden files (e.g. `*.out` files) are generated automatically when missing, and can be updated by setting the "HSPEC_ACCEPT" environment variable, e.g. by running `HSPEC_ACCEPT=true stack test`.

The source map tests' output can be visualized using the [Source Map Visualization](https://sokra.github.io/source-map-visualization/) website. The site requires uploading three files in the following order: the `.js` file, the `.js.map` file, and the `.purs` file.

To produce these files, run `stack test --fast --ta "--match sourcemaps" && ./get-source-maps.sh`. Each test's 3 files will be stored in `.source-maps/<test file name>/` folder. The `get-source-maps.sh` script only works if the test files abide by the requirements described in [TestSourceMaps.hs](.tests/TestSourceMaps.hs).

### Adding Dependencies

Because the PureScript compiler is distributed in binary form, we include the licenses of all dependencies, including transitive ones, in the LICENSE file. Therefore, whenever the dependencies change, the LICENSE file should be updated.

This process can be performed automatically by running `make license-generator`.

### Getting Pull Requests Merged

Sometimes pull requests take a little while to be merged. This is partially because they often have knock-on effects for the rest of the ecosystem, and partially because we want to give core team members time to review and consider changes thoroughly. Please see the organization's [governance document](https://github.com/purescript/governance) for information about when a pull request may be merged.

## Developer Guide

The following instructions are intended to help PureScript users more easily contribute to the compiler, even if this is your first Haskell project.

### Prerequisites

Install `stack`. [Instructions](https://docs.haskellstack.org/en/stable/README/).

Update stack's package index before proceeding:
```
stack update
```

### Clone

```
git clone https://github.com/purescript/purescript.git purescript_compiler
cd purescript_compiler
```

### Build

```
stack build
```

This will take a while the first time it is run.

### Running a locally-compiled version of PureScript

Run `stack exec bash` to launch a subshell (substitute `bash` with your preferred shell) where your locally-compiled version of `purs` is available at the front of your `PATH`. Other tools (such as `spago`) will also grab this latest `purs` version if executed in this shell. You can use `purs --version` and `which purs` to confirm you're executing your locally-compiled version.

```
> purs --version
0.14.2
> which purs
~/.nvm/versions/node/v14.9.0/bin/purs

> stack exec bash

> purs --version
0.14.2 [development build; commit: f1953214775945b65ba53ae903b4238c352dcd29 DIRTY]
> which purs
~/projects/purescript/complier/.stack-work/install/x86_64-linux-tinfo6/1a835accec0abb5a1f7364196133985d18f8c46ee8c7424ce43cf68bab56e5b1/8.10.4/bin/purs
```

If you plan on using your patched version of `purs` for a while (for example, while waiting on your changes to be incorporated into the next official release), it may be more convenient to install it globally with:

```
stack install
```

Note that other installed version (e.g. what npm installs) may still have priority depending on how your `PATH` is configured. `stack install` should warn about other higher-priority versions, and you can always use `which purs` as a sanity check. Uninstall by simply deleting the `purs` binary (location can be found with `which purs`).

### Profiling

A profiling build is used to help diagnose performance issues with the compiler.

Create a profiling build with:
```
stack build --profile
```
This will also take a while the first time it is run.

Setting-up a local shell for your profiling build is similar to the steps for the standard build, just add the `--profile` flag:
```
stack exec --profile bash
```
Note that the bin directory prepended to `$PATH` is different than the standard build, so you can let this be a third "profiling" shell that you leave open between rebuilds.

The `purs` compiler is often wrapped by `spago`. Here's how to pass the "time profiling" flag `-p` via spago:
```
spago build --purs-args "+RTS -p -RTS"
```

Note: There are other profiling flags (such as `-hc` for heap size). You can read more about these flags [here](http://book.realworldhaskell.org/read/profiling-and-optimization.html).

This creates a `purs.prof` file. You can view the contents of this file directly, but it is often more convenient to use a visualizer.

### Profile Visualizers

Each of these produces a clickable visual display of profiling info. Feel free to open the output files in the web browser of your choice. These examples use `firefox`.

#### [ghc-prof-flamegraph](https://github.com/fpco/ghc-prof-flamegraph)
```
stack build --copy-compiler-tool ghc-prof-flamegraph
stack exec -- ghc-prof-flamegraph purs.prof
firefox purs.svg
```

For more flamegraph customizations, you can also try [`stackcollapse-ghc`](https://github.com/marcin-rzeznicki/stackcollapse-ghc)

#### [profiteur](https://github.com/jaspervdj/profiteur)
```
stack build --copy-compiler-tool profiteur
stack exec -- profiteur purs.prof
firefox purs.prof.html
```

### Additional Resources

* [Haskell Language Server](https://github.com/haskell/haskell-language-server#installation) installation guide.

* PureScript-compiler-focused [guide](https://discourse.purescript.org/t/haskell-tooling-guide-vscode-hie/1505) covering VSCode + HIE setup.

* Beginner-friendly [guide](https://www.vacationlabs.com/haskell/environment-setup.html) covering VSCode + HIE setup, although the steps needed some tweaking for compatibility with the PureScript compiler project.

* An [outdated table](https://github.com/rainbyte/haskell-ide-chart#the-chart-with-a-link-to-each-plug-in) of IDE recommendations. Note that the [`intero`](https://github.com/chrisdone/intero/blob/master/README.md) backend (listed for four entries) is no longer supported.
