#!/usr/bin/env bash

# This script assumes `ci/build.sh && cd sdist-test` has been run.

# Creates the following structure
#   Foo.purs
#   src/Bar.purs
#   src/Bar/Baz.purs
#
# and verifies that the two kinds of input globs interact consistently.

set -eu -o pipefail
shopt -s nullglob

PURS="$(stack path --local-doc-root)/../bin/purs"

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT
cd "$tmpdir"

echo ::group::Environment info
echo "purs: ${PURS}"
echo "purs --version"
"${PURS}" --version
echo ::endgroup::

echo ::group::Setting up structure
mkdir -p "src/Bar"

cat > "Foo.purs" <<EOF
module Foo where
foo = 1 :: Int
EOF

cat > "src/Bar.purs" <<EOF
module Bar where
bar = 1 :: Int
EOF

cat > "src/Bar/Baz.purs" <<EOF
module Bar.Baz where
baz = 1 :: Int
EOF

cat > "globsAll" <<EOF
# Generated by Spago. Do not edit!

Foo.purs
src/**/*.purs
test/**/*.purs
EOF

cat > "globsNoFoo" <<EOF
# Generated by Spago. Do not edit!

src/**/*.purs
test/**/*.purs
EOF

(tree . -L 3 || echo "'tree' is not installed, so can't print directory structure")
echo ::endgroup::

echo ::group::Calling purs compile
"${PURS}" compile --output "out1" 'Foo.purs' 'src/**/*.purs' 'test/**/*.purs' 2>&1
EXPECTED=$(cd out1 && tree . 2>&1)

"${PURS}" compile  --output "out2" --source-globs globsAll 2>&1
SOURCE_GLOBS=$(cd out2 && tree . 2>&1)

"${PURS}" compile  --output "out3" --source-globs globsAll 'Foo.purs' 2>&1
MIXED_ALL=$(cd out3 && tree . 2>&1)

"${PURS}" compile  --output "out4" --source-globs globsNoFoo 'Foo.purs' 2>&1
MIXED_NO_FOO=$(cd out4 && tree . 2>&1)
echo ::endgroup::

echo ::group::Running checks
if [ "${EXPECTED}" = "" ] ; then
  echo "'purs compile' output should not be empty"
  exit 1
fi

if [ "${EXPECTED}" = "${SOURCE_GLOBS}" ]; then
  echo "SOURCE_GLOBS is correct"
else
  echo "SOURCE_GLOBS output different from EXPECTED"
  echo "Expected: ${EXPECTED}"
  echo "SOURCE_GLOBS: ${SOURCE_GLOBS}"
  exit 1
fi

if [ "${EXPECTED}" = "${MIXED_ALL}" ]; then
  echo "MIXED_ALL is correct"
else
  echo "MIXED_ALL output different from EXPECTED"
  echo "Expected: ${MIXED_ALL}"
  echo "MIXED_ALL: ${MIXED_ALL}"
  exit 1
fi

if [ "${EXPECTED}" = "${MIXED_NO_FOO}" ]; then
  echo "MIXED_NO_FOO is correct"
else
  echo "MIXED_NO_FOO output different from EXPECTED"
  echo "Expected: ${MIXED_NO_FOO}"
  echo "MIXED_NO_FOO: ${MIXED_NO_FOO}"
  exit 1
fi

echo "Tests passed"
echo ::endgroup::
exit 0
