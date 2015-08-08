## This Windows-specific version of build.sh can be run in an msys environment
## to create a .tar.gz bundle for Windows users.
## msysgit contains all of the pieces needed to run this script.

set -e

SCRIPTPATH=$( cd "$(dirname "$0")" ; pwd -P )

pushd ${SCRIPTPATH}

# Make the staging directory
mkdir -p build/purescript/

# Strip the binaries
strip ../dist/build/psc/psc.exe
strip ../dist/build/psci/psci.exe
strip ../dist/build/psc-docs/psc-docs.exe
strip ../dist/build/psc-publish/psc-publish.exe
strip ../dist/build/psc-bundle/psc-bundle.exe

# Copy files to staging directory
cp ../dist/build/psc/psc.exe                 build/purescript/
cp ../dist/build/psci/psci.exe               build/purescript/
cp ../dist/build/psc-docs/psc-docs.exe       build/purescript/
cp ../dist/build/psc-publish/psc-publish.exe build/purescript/
cp ../dist/build/psc-bundle/psc-bundle.exe   build/purescript/
cp README                                    build/purescript/
cp ../LICENSE                                build/purescript/
cp ../INSTALL.md                             build/purescript/

# Make the binary bundle
pushd build
tar -zcvf ../win64.tar.gz purescript
popd

# Calculate the MD5 hash
md5sum win64.tar.gz > win64.md5

# Remove the staging directory
rm -rf build/

popd

