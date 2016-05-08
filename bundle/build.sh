## This script can be run on any supported OS to create a binary .tar.gz
## bundle.
## For Windows, msysgit contains all of the pieces needed to run this script.
set -e

OS=$1

if [ -z $OS ]
then
  echo "Usage: build.sh osname"
  exit 1
fi

pushd $(stack path --project-root)

LOCAL_INSTALL_ROOT=$(stack path --local-install-root)

if [ "$OS" = "win64" ]
then
  BIN_EXT=".exe"
else
  BIN_EXT=""
fi

# Make the staging directory
mkdir -p bundle/build/purescript

# Strip the binaries, and copy them to the staging directory
for BIN in psc psci psc-docs psc-publish psc-bundle psc-ide-server psc-ide-client
do
  FULL_BIN="$LOCAL_INSTALL_ROOT/bin/${BIN}${BIN_EXT}"
  strip "$FULL_BIN" || true # not the end of the world if this fails, and
                            # AppVeyor can't seem to handle it for some reason
  cp "$FULL_BIN" bundle/build/purescript
done

# Copy extra files to the staging directory
cp bundle/README         bundle/build/purescript/
cp LICENSE               bundle/build/purescript/
cp INSTALL.md            bundle/build/purescript/

stack list-dependencies >bundle/build/purescript/dependencies.txt

# Make the binary bundle
pushd bundle/build > /dev/null
tar -zcvf ../${OS}.tar.gz purescript
popd > /dev/null

# Calculate the SHA hash
if [ "$OS" = "win64" ]
then
  # msys/mingw does not include shasum. :(
  SHASUM="openssl dgst -sha1"
else
  SHASUM="shasum"
fi

$SHASUM bundle/${OS}.tar.gz > bundle/${OS}.sha

# Remove the staging directory
rm -rf build/

popd > /dev/null
