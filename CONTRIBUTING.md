An introductory overview of the compiler is available [here](https://www.youtube.com/watch?v=Y3P1dxqwFiE).

Pull requests are encouraged.

Please follow the following guidelines:

- Add at least a test to `examples/passing/` and possibly to `examples/failing`.
- Build the binaries and libs with `cabal build`
- Install the binaries and libs with `cabal install`.
- Run `cabal configure --enable-tests && cabal build && cabal test` to build the test suite.
- Run `purescript-test-everything` to make sure you haven't broken any core library builds.
- Run `starter-kit` to make sure the starter kit has not been broken.

If you would like to contribute, please consider the issues in the current milestone first.

Finally, if you have made code changes and would like to be included in the copyright notice in the cabal file, please include that change in your pull request.

To prevent core libraries from getting broken, every change must be reviewed. A pull request will be merged as long as one other team member has verified the changes.
