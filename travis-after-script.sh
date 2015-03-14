set -e

pushd core-tests/

./test-everything.sh

popd

if [ -z $( git describe --tags --exact-match 2>/dev/null ) ]
then
  cabal install hpc-coveralls
  case "$SUITE" in
    "tests")
      hpc-coveralls --exclude-dir=dist/build/autogen --exclude-dir=tests tests;;
    "psci-tests")
      hpc-coveralls --exclude-dir=dist/build/autogen --exclude-dir=src --exclude-dir=psci/tests psci-tests;;
    *)
      echo "unrecognised test suite $SUITE"
      exit 1;;
  esac
fi
