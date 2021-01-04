#!/bin/bash
# This script converts the Travis OS name into the format used for PureScript
# binary bundles.
set -e

case "$1" in
  "linux")
    echo linux64;;
  "Linux")
    echo linux64;;
  "osx")
    echo macos;;
  "macOS")
    echo macos;;
  "windows")
    echo win64;;
  "Windows")
    echo win64;;
  *)
    echo "Unknown OS Name: $1";
    exit 1;;
esac
