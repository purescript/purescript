# Installation information

If you are having difficulty installing the PureScript compiler, feel free to ask for help! The best places are the [PureScript Discord](https://purescript.org/chat) or [PureScript Discourse](https://discourse.purescript.org).

## Requirements

The PureScript compiler is built using GHC 9.6.6, and should be able to run on any operating system supported by GHC 9.6.6.
In particular:

* for Windows users, versions predating Vista are not officially supported,
* for macOS / OS X users, versions predating Mac OS X 10.7 (Lion) are not officially supported.

See also <https://www.haskell.org/ghc/download_ghc_9_6_6.html> for more details about the operating systems which GHC 9.6.6 supports.

## Official prebuilt binaries

Each [release](https://github.com/purescript/purescript/releases) comes with prebuilt x86-64 binary bundles for Linux, mac OS, and Windows. Users of other operating systems or architectures will likely need to build the compiler from source; see below.

To install a binary bundle, simply extract it and place the `purs` executable somewhere on your PATH.

## Other distributions

There are several other distributions of the PureScript compiler available, which may be more convenient to use in certain setups. This is by no means an exhaustive list, and is presented in no particular order. Many of these distributions are provided and maintained by the community, and may not be immediately up to date following a new release.

* NPM: `npm install -g purescript`
* Homebrew (for macOS): `brew install purescript`
* FreeBSD binary packages: `pkg install hs-purescript`
* GNU Guix: `guix install purescript`

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

Prior to version v0.14.2, the PureScript REPL depends on the `curses` library
by default (via the Haskell package `terminfo`). If you are having difficulty
running the compiler, it may be because the `curses` library is missing. This
problem may appear as a `libtinfo` error:
```
error while loading shared libraries: libtinfo.so.5: cannot open shared object file: No such file or directory
```

On Linux, you will probably need to install `ncurses` manually. On Ubuntu, for
example, this can be done by running:
```
$ sudo apt install libtinfo5 libncurses5-dev
```

As of v0.14.2, this should no longer be necessary if you are using the prebuilt
binaries or building the compiler from source with the default configuration.
However, you can still opt into using `curses` by setting the Haskeline
`terminfo` flag to `true`. This may improve the REPL experience slightly - for
example, by providing better editing of long input lines.

## EACCES error

If you encounter this error while trying to install via `npm`:
```
Error: EACCES: permission denied
```

The best solution is to install [Node.js and npm via a node version manager](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm#using-a-node-version-manager-to-install-nodejs-and-npm). This error is due to permissions issues when installing packages globally. You can read more about this error in npm's guide to [resolving EACCES permissions errors when installing packages globally](https://docs.npmjs.com/getting-started/fixing-npm-permissions).
