set -e

if [ -z $( git describe --tags --exact-match 2>/dev/null ) ]
then
  cabal configure --enable-tests --enable-coverage -v2
else
  cabal configure --enable-tests -v2
fi

cabal build
cabal test
