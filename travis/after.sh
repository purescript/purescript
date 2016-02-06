set -e

pushd core-tests/
./test-everything.sh
popd

if ! git describe --tags --exact-match >/dev/null 2>/dev/null && [ "$COVERAGE" = "true" ]
then
  ./.cabal-sandbox/bin/hpc-coveralls \
    --exclude-dir=dist/build/autogen \
    --exclude-dir=tests \
    tests
fi
