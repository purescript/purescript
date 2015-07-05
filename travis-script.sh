set -e

if [ -z $( git describe --tags --exact-match 2>/dev/null ) && -n "$COVERAGE_SUITE" ]
then
  cabal configure --enable-tests --enable-coverage -v2
else
  cabal configure --enable-tests -v2
fi

cabal build
cabal test

# Check that a source distribution can be successfully generated, and that
# the generated source distribution can be installed
cabal sdist
SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz &&
   (cd dist && cabal install --force-reinstalls "$SRC_TGZ")
