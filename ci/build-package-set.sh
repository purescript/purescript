#!/usr/bin/env bash

set -eu -o pipefail
shopt -s nullglob

psroot=$(dirname "$(dirname "$(realpath "$0")")")

if [[ "${CI:-}" && "$(echo "$psroot"/CHANGELOG.d/breaking_*)" ]]; then
  echo "Skipping package-set build due to unreleased breaking changes"
  exit 0
fi

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT
export PATH="$tmpdir/node_modules/.bin:$PATH"
cd "$tmpdir"

echo ::group::Ensure Spago is available
which spago || npm install spago
echo ::endgroup::

echo ::group::Create dummy project
spago init --name purescript-dummy
index=$(spago ls paths --json | jq -er '.[] | select(.[0] == "Global registry index path") | .[1]')
# Spago <= 1.0.5 fails on Harmonia 0.1.0's overly long description.
# Spago >= 1.0.6 (currently unreleased) handles it without this workaround.
jq -c 'select(.version != "0.1.0")' "$index/ha/rm/harmonia" > harmonia
mv harmonia "$index/ha/rm/harmonia"
echo ::endgroup::

echo ::group::Compile package set
spago ls packages --json | jq -r 'keys[]' | xargs spago install
echo ::endgroup::

echo ::group::Document package set
spago docs
echo ::endgroup::
