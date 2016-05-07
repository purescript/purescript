#!/bin/bash
# This script converts the Travis OS name into the format used for PureScript
# binary bundles.
set -e

case "$TRAVIS_OS_NAME" in
  "linux")
    echo linux64;;
  "osx")
    echo macos;;
  *)
    echo "Unknown TRAVIS_OS_NAME: $TRAVIS_OS_NAME";
    exit 1;;
esac
