#!/bin/bash

set -ex

# This is the main CI build script. It is intended to run on all platforms we
# run CI on: linux, mac os, and windows. It makes use of the following
# environment variables:
# 
# - CI_RELEASE
# 
#   If set to "true", passes the RELEASE flag to the compiler, and enables
#   optimizations. Otherwise, we disable optimizations (to speed builds up).
# 
# = Stack & GHC
# 
# The script expects `stack setup` to have been run already; if this script is
# run and the correct version GHC is not available, we can end up having stack
# attempt to install all of the compiler's dependencies before the script
# runs. (This is problematic because then the installation won't be subject to
# the timeout mechanism inside the script).
# 
# = Timeouts
# 
# Unfortunately, we are forced to implement a command timeout mechanism for
# this script. At the time of writing:
# 
# - Both CI platforms we use limit the length of time builds may take. Travis
#   CI's limit is 50 minutes, and AppVeyor's is one hour.
# - Setting up GHC and installing all of the compiler's dependencies takes
#   longer than this limit allows.
# 
# Both Travis CI and AppVeyor provide a build cache mechanism, which allows you
# to cache compiled artifacts in order to speed subsequent builds up. However,
# when builds time out, we don't get the opportunity to upload a cache (since
# we've already run out of time). Therefore, if we want the progress we have
# made in a build to be saved to the build cache, we need to make sure we abort
# the build early to allow time to upload the cache. Then, the next commit can
# pick up where the previous commit left off.
# 
# == What to do when a build times out
# 
# If a CI build times out, you need to push a new commit. Amending and
# force-pushing DOES NOT WORK. I suspect this is because CI platforms will only
# consider a particular build cache to be appropriate to use when building a
# given commit with if the cache was created by a parent of the commit being
# built (which is sensible of them).
# 
# = Source distributions
# 
# During a normal build, we create a source distribution with `stack sdist`,
# and then compile and run tests inside that. The reason for this is that it
# helps catch issues arising from forgetting to list files which are necessary
# for compilation or for tests in our package.yaml file (these sorts of issues
# don't test to get noticed until after releasing otherwise).
#
# = Haddock docs
#
# We build with haddock docs because haddock syntax can be malformed and cause
# building HTML docs (for Hackage) to fail; ideally we want to hear about this
# before publishing a release to Hackage.

STACK="stack --no-terminal --jobs=1"

STACK_OPTS="--test --haddock"
if [ "$CI_RELEASE" = "true" ]
then
  STACK_OPTS="$STACK_OPTS --fast"
else
  STACK_OPTS="$STACK_OPTS --flag=purescript:RELEASE"
fi

# Setup & install dependencies or abort
ret=0
if [ -x "/c/msys64/usr/bin/timeout.exe" ]
then
  TIMEOUT=/c/msys64/usr/bin/timeout.exe
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

$TIMEOUT 35m \
  $STACK build --only-dependencies $STACK_OPTS \
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

# Test in a source distribution (see above)
$STACK sdist --tar-dir sdist-test;
tar -xzf sdist-test/purescript-*.tar.gz -C sdist-test --strip-components=1
pushd sdist-test
$STACK build --pedantic $STACK_OPTS
popd
