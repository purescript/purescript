set -e

pushd core-tests/

./test-everything.sh

popd

if [ -z $( git describe --tags --exact-match 2>/dev/null ) ]
then
  cabal install hpc-coveralls
  hpc-coveralls --exclude-dir=tests tests
fi
