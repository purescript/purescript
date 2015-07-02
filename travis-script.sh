set -e

cabal configure --enable-tests -v2
cabal build
cabal test
