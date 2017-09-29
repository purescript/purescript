#!/bin/bash
set -e

STACK="stack --no-terminal --jobs=1"

# Setup & install dependencies or abort
ret=0
$TIMEOUT 40m $STACK --install-ghc build \
  --only-dependencies --test --haddock \
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
if [ -z "$TRAVIS_TAG" ]
then
  # On non-release builds, disable optimizations.
  STACK_EXTRA_FLAGS="--fast"
else
  # On release builds, set the 'release' cabal flag.
  STACK_EXTRA_FLAGS="--flag purescript:RELEASE"
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
