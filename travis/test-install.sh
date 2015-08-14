set -e

# Check that a source distribution can be successfully generated, and that
# the generated source distribution can be installed and tested
cabal sdist
PKGNAME=$(cabal info . | awk '{print $2;exit}')
if SRC_TGZ="$PWD/dist/$PKGNAME.tar.gz"
then
  mkdir -p ../install-test
  cd ../install-test
  mkdir -p sandboxes/$GHCVER/${STACKAGE:-none}
  cabal sandbox init --sandbox sandboxes/$GHCVER/${STACKAGE:-none}

  if [ `echo "$STACKAGE" | sed 's/\..*$//'` = "lts-3" ]; then
	tar -xzf i--strip-components=1 $PKGNAME.tar.gz
	cabal install
	cabal test
  else
    cabal install "$SRC_TGZ"
  fi
fi
