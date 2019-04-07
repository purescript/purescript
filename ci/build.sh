#!/bin/bash
set -e

# This is the main CI build script. It is intended to run on all platforms we
# run CI on: linux, mac os, and windows (via msys). It makes use of the
# following environment variables:
#
# BUILD_TYPE
# Must be one of the following:
#  - "normal": Compile & run tests normally
#  - "sdist": Create a source distribution and check that everything still
#    compiles and works
#  - "haddock": Check that haddock documentation builds correctly.
#
# CI_RELEASE
# If set to "true", passes the RELEASE flag to the compiler and enables
# optimizations.

STACK="stack --no-terminal --jobs=1"
[[ "$BUILD_TYPE" == "haddock" ]] && DEPS_HADDOCK="--haddock"

# This command is ludicrously verbose on Windows, so we pipe the output to a
# file and only display it if the command fails.
if ! $STACK --verbosity=error setup 1>stack-setup.log 2>&1
then
  cat stack-setup.log
  echo "Failed to run 'stack setup'"
  exit 1
fi

# Setup & install dependencies or abort
ret=0
if [ -x "C:\\msys64\\usr\\bin\\timeout.exe" ]
then
  TIMEOUT=C:\\msys64\\usr\\bin\\timeout.exe
elif which timeout >/dev/null
then
  TIMEOUT=timeout
elif which gtimeout >/dev/null
then
  TIMEOUT=gtimeout
else
  echo "timeout command not found (nor gtimeout)"
  exit 1
fi
$TIMEOUT 40m $STACK build \
  --only-dependencies --test $DEPS_HADDOCK \
  || ret=$?
case "$ret" in
  0) # continue
    ;;
  124)
    echo "Timed out while installing dependencies."
    echo "Try pushing a new commit to build again."
    exit 1
    ;;
  *)
    echo "Failed to install dependencies."
    exit 1
    ;;
esac

# Set up configuration
STACK_EXTRA_FLAGS=""
if [ "$CI_RELEASE" = "true" ]
then
  # On release builds, set the 'release' cabal flag.
  STACK_EXTRA_FLAGS="--flag purescript:RELEASE"
else
  # On non-release builds, disable optimizations.
  STACK_EXTRA_FLAGS="--fast"
fi

if [ "$STACKAGE_NIGHTLY" = "true" ]
then
  STACK_EXTRA_FLAGS="$STACK_EXTRA_FLAGS --resolver=nightly"
fi

if [ "$COVERAGE" = "true" ]
then
  STACK_EXTRA_FLAGS="$STACK_EXTRA_FLAGS --coverage"
fi

echo "STACK_EXTRA_FLAGS=\"$STACK_EXTRA_FLAGS\""
BUILD_COMMAND="$STACK build --pedantic --test $STACK_EXTRA_FLAGS"

if [ "$BUILD_TYPE" = "normal" ]
then
  echo ">>> Building & testing..."
  echo "> $BUILD_COMMAND"
  $BUILD_COMMAND

elif [ "$BUILD_TYPE" = "sdist" ]
then
  echo ">>> Testing the source distribution..."
  $STACK sdist
  mkdir sdist-test
  tar -xzf $(stack path --dist-dir)/purescript-*.tar.gz -C sdist-test --strip-components=1
  pushd sdist-test
  echo "> $BUILD_COMMAND"
  $BUILD_COMMAND
  popd

elif [ "$BUILD_TYPE" = "haddock" ]
then
  echo ">>> Checking haddock documentation..."
  $STACK haddock --fast
else
  echo "Unrecognised BUILD_TYPE: $BUILD_TYPE"
  exit 1
fi

if [ "$COVERAGE" = "true" ]
then
  echo ">>> Uploading test coverage report..."
  which shc || $STACK install stack-hpc-coveralls
  shc purescript tests || echo "Failed to upload coverage"
fi
