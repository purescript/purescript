An introductory overview of the compiler is available [here](https://www.youtube.com/watch?v=Y3P1dxqwFiE).

Pull requests are encouraged.

## Finding Issues to Work On

If you would like to contribute, please consider the issues in the current milestone first. If you are a new contributor, you may want to have a go at the ["easy" issues](https://github.com/purescript/purescript/labels/easy) to get started.

## Pull Requests

Please follow the following guidelines:

- Add at least a test to `examples/passing/` and possibly to `examples/failing/`.
- Build the binaries and libs with `stack build`
- Make sure that all test suites are passing. Run the test suites with `stack test`.
- Build the core libraries by running the script in `core-tests`.

## Tests

Run all test suites with `stack test`. You will need `npm`, `bower` and `node` on your PATH to run the tests.

To build and run a specific test in `examples/passing/` or `examples/failing/`, execute the following commands.

``` bash
# Build
stack exec psc -- 'tests/support/bower_components/purescript-*/src/**/*.purs' examples/blah/Blah.purs

# Run
node -e "require('./output/Main/').main()"
```

## Code Review

To prevent core libraries from getting broken, every change must be reviewed. A pull request will be merged as long as one other team member has verified the changes.

## Adding Dependencies

Because the PureScript compiler is distributed in binary form, we include
the licenses of all dependencies, including transitive ones, in the LICENSE
file. Therefore, whenever the dependencies change, the LICENSE file should be
updated.

This can be automated; see the `license-generator/generate.hs` file.

## Writing Issues

- If the issue is actually a question, please consider asking on Reddit, Stack Overflow or IRC first.
- Please include a minimal, repeatable test case with any bug report.

## Copyright and Licensing

For any code change, please append a copyright and licensing notice to the [CONTRIBUTORS.md](CONTRIBUTORS.md) file.
