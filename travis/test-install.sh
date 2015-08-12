set -e

# Check that a source distribution can be successfully generated, and that
# the generated source distribution can be installed
cabal sdist
if SRC_TGZ="$PWD/dist/$(cabal info . | awk '{print $2;exit}').tar.gz"
then
  mkdir -p ../install-test
  cd ../install-test
  mkdir -p sandboxes/$GHCVER/${STACKAGE:-none}
  cabal sandbox init --sandbox sandboxes/$GHCVER/${STACKAGE:-none}
  cabal install -j2 --ghc-options='+RTS -A32m -RTS' "$SRC_TGZ"
fi
