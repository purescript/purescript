# Installation information

If you are having difficulty installing the PureScript compiler, feel free to
ask for help! A good place is the #purescript IRC channel on Freenode, or
alternatively Stack Overflow.

## Using prebuilt binaries

The prebuilt binaries are compiled with GHC 7.10.3, and therefore they should
run on any operating system supported by GHC 7.10.3, such as:

* Windows 2000 or later,
* OS X 10.7 or later,
* Linux ??? (we're not sure what the minimum version is).

This list is not exhaustive. If your OS is too old or not listed, or if the
binaries fail to run, you may be able to install the compiler by building it
from source; see below.

It's probably safe to assume that other prebuilt distributions (eg, Homebrew,
Chocolatey, AUR, npm) use the same binaries, and therefore have the same
requirements.

## Compiling from source

GHC 7.10.1 or newer is required to compile from source. The easiest way is to
use stack:

```
$ stack update
$ stack unpack purescript
$ cd purescript-x.y.z  # (replace x.y.z with whichever version you just downloaded)
$ stack install
```

This will then copy the compiler and utilities into `~/.local/bin`.


If you don't have stack installed, there are install instructions
[here](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md).

If you don't have ghc installed, stack will prompt you to run `stack setup`
which will install ghc for you.

## The "curses" library

`psci` depends on the `curses` library (via the Haskell package `terminfo`). If
you are having difficulty running the compiler, it may be because the `curses`
library is missing.

On Linux, you will probably need to install `ncurses` manually. On Ubuntu, for
example, this can be done by running:

```
$ sudo apt-get install libncurses5-dev
```
