set -e

pushd core-tests/
./test-everything.sh
popd

if ! git describe --tags --exact-match >/dev/null 2>/dev/null && [ -n "$COVERAGE_SUITE" ]
then
  case "$COVERAGE_SUITE" in
    "tests")
      ./.cabal-sandbox/bin/hpc-coveralls \
         --exclude-dir=dist/build/autogen \
         --exclude-dir=tests \
         tests;;
    "psci-tests")
      ./.cabal-sandbox/bin/hpc-coveralls \
          --exclude-dir=dist/build/autogen \
          --exclude-dir=src \
          --exclude-dir=psci/tests \
          psci-tests;;
    *)
      echo "unrecognised test suite $COVERAGE_SUITE"
      exit 1;;
  esac
fi
