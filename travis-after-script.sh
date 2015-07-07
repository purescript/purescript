set -e

pushd core-tests/

./test-everything.sh

popd

if [ -z $( git describe --tags --exact-match 2>/dev/null ) && -n "$COVERAGE_SUITE" ]
then
  cabal install hpc-coveralls
  case "$COVERAGE_SUITE" in
    "tests")
      hpc-coveralls --exclude-dir=dist/build/autogen --exclude-dir=tests tests;;
    "psci-tests")
      hpc-coveralls --exclude-dir=dist/build/autogen --exclude-dir=src --exclude-dir=psci/tests psci-tests;;
    *)
      echo "unrecognised test suite $COVERAGE_SUITE"
      exit 1;;
  esac
fi
