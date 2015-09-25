An introductory overview of the compiler is available [here](https://www.youtube.com/watch?v=Y3P1dxqwFiE).

Pull requests are encouraged.

## Finding Issues to Work On

If you would like to contribute, please consider the issues in the current milestone first. If you are a new contributor, you may want to have a go at the ["easy" issues](https://github.com/purescript/purescript/labels/easy) to get started.

## Pull Requests

Please follow the following guidelines:

- Add at least a test to `examples/passing/` and possibly to `examples/failing`.
- Build the binaries and libs with `cabal build`
- Install the binaries and libs with `cabal install`.
- Run `cabal configure --enable-tests && cabal build && cabal test` to build the test suite. You will need `npm` and `node` on your PATH to run the tests.
- Build the core libraries by running the script in `core-tests`.

## Code Review

To prevent core libraries from getting broken, every change must be reviewed. A pull request will be merged as long as one other team member has verified the changes.

## Adding Dependencies

Because the PureScript compiler is distributed in binary form, we include
the licenses of all dependencies, including transitive ones, in the LICENSE
file. Therefore, whenever the dependencies change, the LICENSE file should be
updated.

You can automate this (if you have bash):

- get a copy of [cabal-dependency-licenses][]
- run at the command line: `./license/generate > LICENSE`

[cabal-dependency-licenses]: https://github.com/jaspervdj/cabal-dependency-licenses

## Writing Issues

- If the issue is actually a question, please consider asking on Reddit, Stack Overflow or IRC first.
- Please include a minimal, repeatable test case with any bug report.

## Copyright and Licensing

For any code change, please append a copyright and licensing notice to the [CONTRIBUTORS.md](CONTRIBUTORS.md) file.
