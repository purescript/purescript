set -e

SCRIPTPATH=$( cd "$(dirname "$0")" ; pwd -P )

OS=$( uname )

pushd ${SCRIPTPATH} > /dev/null

# Make the staging directory
mkdir -p build/purescript/

# Strip the binaries
strip ../dist/build/psc/psc
strip ../dist/build/psci/psci
strip ../dist/build/psc-make/psc-make
strip ../dist/build/docgen/docgen

# Copy files to staging directory
cp ../dist/build/psc/psc           build/purescript/
cp ../dist/build/psci/psci         build/purescript/
cp ../dist/build/psc-make/psc-make build/purescript/
cp ../dist/build/docgen/docgen     build/purescript/
cp ../prelude/prelude.purs         build/purescript/
cp install.sh                      build/purescript/

# Make the binary bundle
pushd build > /dev/null
tar -zcvf ../$OS.tar.gz purescript
popd > /dev/null

# Calculate the SHA hash
shasum $OS.tar.gz > $OS.sha

# Remove the staging directory
rm -rf build/

popd > /dev/null
