## This Windows-specific version of build.sh can be run in an msys environment
## to create a .tar.gz bundle for Windows users.
## msysgit contains all of the pieces needed to run this script.

set -e

SCRIPTPATH=$( cd "$(dirname "$0")" ; pwd -P )

pushd ${SCRIPTPATH} > /dev/null

# Make the staging directory
mkdir -p build/purescript/

# Strip the binaries
strip $APPDATA/local/bin/psc.exe
strip $APPDATA/local/bin/psci.exe
strip $APPDATA/local/bin/psc-docs.exe
strip $APPDATA/local/bin/psc-publish.exe
strip $APPDATA/local/bin/psc-bundle.exe
strip $APPDATA/local/bin/psc-ide-server.exe
strip $APPDATA/local/bin/psc-ide-client.exe

# Copy files to staging directory
cp $APPDATA/local/bin/psc.exe            build/purescript/
cp $APPDATA/local/bin/psci.exe           build/purescript/
cp $APPDATA/local/bin/psc-docs.exe       build/purescript/
cp $APPDATA/local/bin/psc-publish.exe    build/purescript/
cp $APPDATA/local/bin/psc-bundle.exe     build/purescript/
cp $APPDATA/local/bin/psc-ide-server.exe build/purescript/
cp $APPDATA/local/bin/psc-ide-client.exe build/purescript/
cp README                                build/purescript/
cp ../LICENSE                            build/purescript/
cp ../INSTALL.md                         build/purescript/

# Make the binary bundle
pushd build > /dev/null
tar -zcvf ../win64.tar.gz purescript
popd > /dev/null

# Calculate the MD5 hash
md5sum win64.tar.gz > win64.md5

# Remove the staging directory
rm -rf build/

popd > /dev/null
