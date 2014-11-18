set -e

if [ -z $( git describe --tags --exact-match 2>/dev/null ) ]
then
  cabal install hpc-coveralls
  hpc-coveralls --exclude-dir=tests tests
fi
