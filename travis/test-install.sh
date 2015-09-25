set -e

# Check that a source distribution can be successfully generated, and that
# the generated source distribution can be installed and tested
cabal sdist
if SRC_TGZ="$PWD/dist/$(cabal info . | awk '{print $2;exit}').tar.gz"
then
  if [ "$RUNSDISTTESTS" = "YES" ]; then
    mkdir test-install
    cd test-install
    tar --strip-components=1 -xzf $SRC_TGZ
    cabal sandbox init --sandbox ../.cabal-sandbox
    cabal install --enable-tests --force-reinstalls
    cabal test
  else
    cabal install "$SRC_TGZ"
  fi
fi
