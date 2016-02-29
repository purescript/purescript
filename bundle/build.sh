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
strip ~/.local/bin/psc
strip ~/.local/bin/psci
strip ~/.local/bin/psc-docs
strip ~/.local/bin/psc-publish
strip ~/.local/bin/psc-bundle
strip ~/.local/bin/psc-ide-server
strip ~/.local/bin/psc-ide-client

# Copy files to staging directory
cp ~/.local/bin/psc            build/purescript/
cp ~/.local/bin/psci           build/purescript/
cp ~/.local/bin/psc-docs       build/purescript/
cp ~/.local/bin/psc-publish    build/purescript/
cp ~/.local/bin/psc-bundle     build/purescript/
cp ~/.local/bin/psc-ide-server build/purescript/
cp ~/.local/bin/psc-ide-client build/purescript/
cp README                      build/purescript/
cp ../LICENSE                  build/purescript/
cp ../INSTALL.md               build/purescript/

# Make the binary bundle
pushd build > /dev/null
tar -zcvf ../$OS.tar.gz purescript
popd > /dev/null

# Calculate the SHA hash
shasum $OS.tar.gz > $OS.sha

# Remove the staging directory
rm -rf build/

popd > /dev/null
