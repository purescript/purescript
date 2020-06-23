# Developer Guide

These instructions are intended to help PureScript users more easily contribute to the compiler, even if this is your first Haskell project.

## Prerequisites

Install `stack`. [Instructions](https://docs.haskellstack.org/en/stable/README/).

See the [PureScript Install Guide](INSTALL.md) for information on the `curses`/`ncurses` dependency.

Update `stack` before proceeding:
```
stack update
```

## Select Release

After cloning the PureScript compiler repo, `checkout` the tagged release that you'd like to build.

For example:
```
git checkout v0.13.8
```

Note that you can view the latest tags with:
```
git log --oneline --tags
```

You may also skip this tagged checkout and build the unreleased `master` branch instead.

Another option is to "unpack" the latest release locally. This creates another directory containing the code for that release, but this code is not tracked by git, so it is less convenient if you are planning on making changes:

```
stack unpack purescript
cd purescript-x.y.z  # (replace x.y.z to match the directory just created via unpack)
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
This will also take a while the first time it is run. Incremental builds are faster, and you may swap between standard and profiling builds without a rebuild penalty as work for each is saved in separate directories.

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

## Haskell IDE

There are many Haskell IDE options. Feel free to contribute instructions for your preferred editor.

Here are instructions for getting VSCode + HIE backend configured for this PureScript compiler project.

Install [VSCode](https://code.visualstudio.com/) and the [vscode-hie-server](https://github.com/alanz/vscode-hie-server) extension.

### Setup [HIE](https://github.com/haskell/haskell-ide-engine)
This needs to be built from source. You should also take a look at the [readme](https://github.com/haskell/haskell-ide-engine/blob/master/README.md#installation) to check for any OS-specific instructions. These steps for for Ubuntu 20.04.

Clone the code:
```
git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
cd haskell-ide-engine
```

Find the GHC version that your branch of the PureScript compiler is using. This requires navigating to the compiler's source directory and running:
```
$ stack ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 8.6.5
```
Note that this version will likely be different if this command is run from another location, such as your home directory.

Then, back in the `haskell-ide-engine` source directory, install for that version of the compiler. For example:
```
stack ./install.hs hie-8.6.5
```
This command will take a while to complete.

You can then optionally generate a Hoogle database to provide pop-up documentation when hovering over a symbol in VSCode. Run this command from the same `haskell-ide-engine` directory.
```
stack --stack-yaml=stack-8.6.5.yaml exec hoogle generate
```

### Other Tools

Optionally install [`hlint`](https://github.com/ndmitchell/hlint) for warnings/errors, [`apply-refact`](https://github.com/mpickering/apply-refact) for quick-fix suggestions, and [`brittany`](https://github.com/lspitzner/brittany) for code formatting. If these tools are installed, then their additional functionality will be picked-up by the VSCode extension.
```
stack install hlint apply-refact brittany
```

### Running VSCode

Then launch VSCode from the root of the PureScript compiler directory.
```
cd <purescript compiler directory>
code .
```

You can read more about the [available extension features here](https://github.com/alanz/vscode-hie-server#features).

## Additional Resources

* Beginner-friendly [guide](https://www.vacationlabs.com/haskell/environment-setup.html) covering VSCode + HIE setup, although the steps needed some tweaking for compatibility with the PureScript compiler project.

* An [outdated table](https://github.com/rainbyte/haskell-ide-chart#the-chart-with-a-link-to-each-plug-in) of IDE recommendations. Note that the [`intero`](https://github.com/chrisdone/intero/blob/master/README.md) backend (listed for four entries) is no longer supported.
