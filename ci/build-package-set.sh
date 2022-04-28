#!/usr/bin/env bash

set -eu -o pipefail
shopt -s nullglob

psroot=$(dirname "$(dirname "$(realpath "$0")")")

if [[ "${CI:-}" && "$(echo $psroot/CHANGELOG.d/breaking_*)" ]]; then
  echo "Skipping package-set build due to unreleased breaking changes"
  exit 0
fi

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT
export PATH="$tmpdir/node_modules/.bin:$PATH"
cd "$tmpdir"

echo ::group::Ensure Spago is available
SPAGO="spago"
if [ "${CI:-}" ] && [ ! -x "$(command -v spago 2>/dev/null)" ]; then
  echo "Installing spago"
  SPAGO_VERSION=0.20.8
  curl -o spago.tar.gz -L "https://github.com/purescript/spago/releases/download/$SPAGO_VERSION/Linux.tar.gz"
  tar -xvf spago.tar.gz
  chmod +x spago
  SPAGO="./spago"
  "$SPAGO" --version
fi
echo ::endgroup::

echo ::group::Create dummy project
echo 'let upstream = https://github.com/purescript/package-sets/releases/download/XXX/packages.dhall in upstream' > packages.dhall
echo '{ name = "my-project", dependencies = [] : List Text, packages = ./packages.dhall, sources = [] : List Text }' > spago.dhall
"$SPAGO" upgrade-set
# Override the `metadata` package's version to match `purs` version
# so that `spago build` actually works
sed -i'' "\$c in upstream with metadata.version = \"v$(purs --version | { read v z && echo $v; })\"" packages.dhall
"$SPAGO" install $("$SPAGO" ls packages | while read name z; do echo $name; done)
echo ::endgroup::

echo ::group::Compile package set
"$SPAGO" build
echo ::endgroup::

echo ::group::Document package set
"$SPAGO" docs --no-search
echo ::endgroup::
