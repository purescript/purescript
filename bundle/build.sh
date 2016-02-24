set -e

SCRIPTPATH=$( cd "$(dirname "$0")" ; pwd -P )

OS=$1

if [ -z $OS ]
then
  echo "Usage: build.sh osname"
  exit 1
fi

pushd ${SCRIPTPATH} > /dev/null

# Make the staging directory
mkdir -p build/purescript/

# Strip the binaries
strip ../dist/build/psc/psc
strip ../dist/build/psci/psci
strip ../dist/build/psc-docs/psc-docs
strip ../dist/build/psc-publish/psc-publish
strip ../dist/build/psc-bundle/psc-bundle
strip ../dist/build/psc-ide-server/psc-ide-server
strip ../dist/build/psc-ide-client/psc-ide-client

# Copy files to staging directory
cp ../dist/build/psc/psc                       build/purescript/
cp ../dist/build/psci/psci                     build/purescript/
cp ../dist/build/psc-docs/psc-docs             build/purescript/
cp ../dist/build/psc-publish/psc-publish       build/purescript/
cp ../dist/build/psc-bundle/psc-bundle         build/purescript/
cp ../dist/build/psc-ide-server/psc-ide-server build/purescript/
cp ../dist/build/psc-ide-client/psc-ide-client build/purescript/
cp README                                      build/purescript/
cp ../LICENSE                                  build/purescript/
cp ../INSTALL.md                               build/purescript/

# Make the binary bundle
pushd build > /dev/null
tar -zcvf ../$OS.tar.gz purescript
popd > /dev/null

# Calculate the SHA hash
shasum $OS.tar.gz > $OS.sha

# Remove the staging directory
rm -rf build/

popd > /dev/null
