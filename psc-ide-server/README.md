psc-ide
===

A tool which provides editor support for the PureScript programming language.

## Editor Integration
* [@epost](https://github.com/epost) wrote a plugin to integrate psc-ide with Emacs at https://github.com/epost/psc-ide-emacs.
* Atom integration is available with https://github.com/nwolverson/atom-ide-purescript.
* Visual Studio Code integration is available with https://github.com/nwolverson/vscode-ide-purescript.
* Vim integration is available here: https://github.com/FrigoEU/psc-ide-vim.

## Running the Server

Start the server by running the `psc-ide-server [SOURCEGLOBS]` executable, where
`SOURCEGLOBS` are (optional) globs that match your PureScript sourcefiles.

It supports the following options:

- `-p / --port` specify a port. Defaults to 4242
- `-d / --directory` specify the toplevel directory of your project. Defaults to
  the current directory
- `--output-directory`: Specify where to look for compiled output inside your
  project directory. Defaults to `output/`, relative to either the current
  directory or the directory specified by `-d`.
- `--debug`: Enables some logging meant for debugging
- `--no-watch`: Disables the filewatcher
- `--version`: Output psc-ide version

## Issuing queries

After you started the server you can start issuing requests using
`psc-ide-client`. Make sure you start by loading the modules before you try to
query them.

`psc-ide-server` expects the build externs.purs inside the `output/` folder of
your project after running `pulp build` or `psc-make` respectively.

(If you changed the port of the server you can change the port for
`psc-ide-client` by using the -p option accordingly)

## Protocol

For documentation about the protocol have a look at:
[PROTOCOL.md](PROTOCOL.md)
