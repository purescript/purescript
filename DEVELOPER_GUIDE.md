# Developer Guide

These instructions are intended to help PureScript users more easily contribute to the compiler, even if this is your first Haskell project.

## Prerequisites

Install `stack`. [Instructions](https://docs.haskellstack.org/en/stable/README/).

See the [PureScript Install Guide](INSTALL.md) for information on the `curses`/`ncurses` dependency.

Update stack's package index before proceeding:
```
stack update
```

## Clone

```
git clone https://github.com/purescript/purescript.git purescript_compiler
cd purescript_compiler
```

## Build

```
stack build
```

This will take a while the first time it is run.

## Install

If you'd like to test the build in a local shell, you can run:
```
stack exec bash
```

This prepends the location of the compiler to your `$PATH`, so you can test your changes. This works with other tools that depend on the `purs` binary, such as `spago`. Note that rebuilds continue to use this same path, so you don't need to keep re-running `stack exec` to test new changes (assuming you're using a separate terminal for `stack exec`). Remember that `stack exec` must be launched from the same directory that you ran `stack build` from.

You may also install globally with:
```
stack install
```

This copies the compiler executable (`purs`) into `~/.local/bin`.

## Profiling

A profiling build is used to help diagnose performance issues with the compiler.

Create a profiling build with:
```
stack build --profile
```
This will also take a while the first time it is run. Incremental builds are faster. If you are using stack 2.x, you may swap between standard and profiling builds without a rebuild penalty as work for each is saved in separate directories.

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

One nice `.prof` visualizer is [`profiteur`](https://github.com/jaspervdj/profiteur).
```
stack install profiteur
profiteur purs.prof
```
Then open the generated `purs.prof.html` in your web browser.

## Testing

See the [Contributing](CONTRIBUTING.md) readme for testing instructions.

## Additional Resources

* PureScript-compiler-focused [guide](todo) covering VSCode + HIE setup.

* Beginner-friendly [guide](https://www.vacationlabs.com/haskell/environment-setup.html) covering VSCode + HIE setup, although the steps needed some tweaking for compatibility with the PureScript compiler project.

* An [outdated table](https://github.com/rainbyte/haskell-ide-chart#the-chart-with-a-link-to-each-plug-in) of IDE recommendations. Note that the [`intero`](https://github.com/chrisdone/intero/blob/master/README.md) backend (listed for four entries) is no longer supported.
