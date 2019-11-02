# Installation information

If you are having difficulty installing the PureScript compiler, feel free to
ask for help! A good place is the #purescript IRC channel on Freenode, the #purescript channel on [FPChat Slack](https://fpchat-invite.herokuapp.com/), or
alternatively Stack Overflow.

## Requirements

The PureScript compiler is built using GHC 8.6.4, and should be able to run on any operating system supported by GHC 8.6.4. In particular:

* for Windows users, versions predating Vista are not officially supported,
* for macOS / OS X users, versions predating Mac OS X 10.7 (Lion) are not officially supported.

See also <https://www.haskell.org/ghc/download_ghc_8_6_4.html> for more details about the operating systems which GHC 8.6.4 supports.

## Official prebuilt binaries

Each release comes with prebuilt x86-64 binary bundles for Linux, mac OS, and Windows. Users of other operating systems or architectures will likely need to build the compiler from source; see below.

To install a binary bundle, simply extract it and place the `purs` executable somewhere on your PATH.

## Other distributions

There are several other distributions of the PureScript compiler available, which may be more convenient to use in certain setups. This is by no means an exhaustive list, and is presented in no particular order. Many of these distributions are provided and maintained by the community, and may not be immediately up to date following a new release.

* NPM: `npm install -g purescript`
* Homebrew (for macOS): `brew install purescript`
* [PSVM](https://github.com/ThomasCrevoisier/psvm-js): `npm install -g psvm`

## Compiling from source

The easiest way is to use stack:

```
$ stack update
$ stack unpack purescript
$ cd purescript-x.y.z  # (replace x.y.z with whichever version you just downloaded)
$ stack install --flag purescript:RELEASE
```

This will then copy the compiler executable (`purs`) into `~/.local/bin`.

If you don't have stack installed, please see the [stack install documentation](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md).

## The "curses" library

The PureScript REPL depends on the `curses` library (via the Haskell package
`terminfo`). If you are having difficulty running the compiler, it may be
because the `curses` library is missing.

On Linux, you will probably need to install `ncurses` manually. On Ubuntu, for
example, this can be done by running:

```
$ sudo apt-get install libncurses5-dev
```
