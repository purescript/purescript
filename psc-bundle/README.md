# psc-bundle

A dead code elimination tool for PureScript-style CommonJS modules. This can be used as an alternative to Browserify.

## Usage

    psc-bundle FILE (-m|--module ARG) [--main ARG] [--namespace ARG] [--optimize uncurry]

Options:

- The input .js file(s)
- Entry point module name(s) are specified with `-m` or `--module`. All code which is not a transitive dependency of an entry point module will be removed.
- The main module is (optionally) specified using `--main`. If specified, this will generate code to run the main method in the specified module.
- The browser namespace defaults to `PS`, and can be overridden with `--namespace`.
- The uncurry optimization option is off by default. It can be explicitly enabled with `-O` or `--optimize` and disabled with `--no-optimize`.

For example, to bundle the modules in the `output` directory, with main module `Main`:

    psc-bundle output/**/*.js -m Main --main Main
