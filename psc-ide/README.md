purs ide
===

Editor and tooling support for the PureScript programming language.

## Setting up your editor

This document will describe how to run `purs ide` as an editor plugin creator.
If you're looking to set up your PureScript development environment consult
the
[documentation repository](https://github.com/purescript/documentation/blob/master/ecosystem/Editor-and-tool-support.md) instead.

## Running the Server

Start the server by running the `purs ide server [SOURCEGLOBS]` executable, where
`SOURCEGLOBS` are (optional) globs that match your PureScript sourcefiles.

It supports the following options:

- `-p / --port` specify a port. Defaults to 4242
- `-d / --directory` specify the toplevel directory of your project. Defaults to
  the current directory
- `--output-directory`: Specify where to look for compiled output inside your
  project directory. Defaults to `output/`, relative to either the current
  directory or the directory specified by `-d`.
- `--polling`: Uses polling instead of file system events to watch the externs
  files. This flag is reversed on Windows and polling is the default.
- `--log-level`: Can be set to one of "all", "none", "debug" and "perf"
- `--no-watch`: Disables the filewatcher
- `--editor-mode`: Only reload on source file changes reported by the editor
- `--version`: Output psc-ide version

## Issuing queries

After you started the server you can start issuing requests using
`purs ide client`. Make sure you start by loading the modules before you try to
query them.

`purs ide` expects the built externs.json inside the output folder of your
project after running `pulp build` or `purs compile` respectively.

(If you changed the port of the server you can change the port for
`purs ide client` by using the -p option accordingly)

## Protocol

If you want to know how to send commands/queries to `purs ide` take a look
at [PROTOCOL.md](PROTOCOL.md)
