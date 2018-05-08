# Installation information

If you are having difficulty installing the PureScript compiler, feel free to
ask for help! A good place is the #purescript IRC channel on Freenode, the #purescript channel on [FPChat Slack](https://fpchat-invite.herokuapp.com/), or
alternatively Stack Overflow.

## Using prebuilt binaries

The prebuilt binaries are compiled with GHC 8.2.2 and therefore they should
run on any operating system supported by GHC 8.2.2, such as:

* Windows Vista or later,
* OS X 10.7 or later,
* Linux ??? (we're not sure what the minimum version is)

This list is not exhaustive. If your OS is too old or not listed, or if the binaries fail to run, you may be able to install the compiler by building it from source; see below. See also <https://www.haskell.org/ghc/download_ghc_8_2_2.html> for more details about the operating systems which GHC 8.2.2 supports.

Other prebuilt distributions (eg, Homebrew, AUR, npm) will probably have the
same requirements.

## Installing a pre-built distribution

There are several options available for aquiring a pre-built binary of the PureScript compiler.  This is by no means an exhaustive list, and is presented in no particular order. Each example is expected to install the latest available compiler version at the time of running the command. Many of these are provided and maintained by the community, and may not be immediately up to date.

* NPM: `npm install -g purescript`
* Homebrew (for OS X): `brew install purescript`
* [PSVM](https://github.com/ThomasCrevoisier/psvm-js) (PS Version Manager): 
  1) `psvm install-latest` will install the latest version available
  2) `psvm latest` will print the latest version number available
  3) `psvm use <latest version number>` will enable the version we just installed. For example, if the version is `v0.11.7`, you'd run `psvm use v0.11.7`

## Compiling from source

The easiest way is to use stack:

```
$ stack update
$ stack unpack purescript
$ cd purescript-x.y.z  # (replace x.y.z with whichever version you just downloaded)
$ stack install --flag purescript:RELEASE
```

This will then copy the compiler and utilities into `~/.local/bin`.


If you don't have stack installed, there are install instructions
[here](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md).

If you don't have GHC installed, stack will prompt you to run `stack setup`
which will install the correct version of GHC for you.

## The "curses" library

The PureScript REPL depends on the `curses` library (via the Haskell package
`terminfo`). If you are having difficulty running the compiler, it may be
because the `curses` library is missing.

On Linux, you will probably need to install `ncurses` manually. On Ubuntu, for
example, this can be done by running:

```
$ sudo apt-get install libncurses5-dev
```
