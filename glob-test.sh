#!/usr/bin/env sh

# Creates the following structure
# z/Foo.purs
# z/src/Bar.purs
# z/src/Bar/Baz.purs

mkdir -p z/src/Bar

cat > z/Foo.purs <<EOF
module Foo where
foo = 1 :: Int
EOF

cat > z/src/Bar.purs <<EOF
module Bar where
bar = 1 :: Int
EOF

cat > z/src/Bar/Baz.purs <<EOF
module Bar.Baz where
baz = 1 :: Int
EOF

cat > z/globsAll <<EOF
# Generated by Spago. Do not edit!

z/Foo.purs
z/src/**/*.purs
z/test/**/*.purs
EOF

cat > z/globsNoFoo <<EOF
# Generated by Spago. Do not edit!

z/src/**/*.purs
z/test/**/*.purs
EOF

echo "env info"
purs --version
tree z -L 3
echo "---"
cat z/Foo.purs
echo "---"
cat z/src/Bar.purs
echo "---"
cat z/src/Bar/Baz.purs
echo "---"

EXPECTED=$(purs compile --output z/out1 'z/Foo.purs' 'z/src/**/*.purs' 'z/test/**/*.purs' 2>&1)
SOURCE_GLOBS=$(purs compile  --output z/out2 --source-globs z/globsAll 2>&1)
MIXED_ALL=$(purs compile  --output z/out3 --source-globs z/globsAll 'z/Foo.purs' 2>&1)
MIXED_NO_FOO=$(purs compile  --output z/out4 --source-globs z/globsNoFoo 'z/Foo.purs' 2>&1)

echo "Result"
tree z/out1
tree z/out2
tree z/out3
tree z/out4

rm -rf z/

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
exit 0
