#!/usr/bin/env bash

TEST_MODULES_DIR=.test_modules
OUTPUT_DIR=.source-maps

if [ ! -d "$TEST_MODULES_DIR" ]; then
  echo "'$TEST_MODULES_DIR' dir does not exist. You need to run 'stack test --fast --ta \"match sourcemaps\"' first"
  exit 1
fi

if [ -d "$OUTPUT_DIR" ]; then
  echo "Removing $OUTPUT_DIR"
  rm -rf "$OUTPUT_DIR"
fi

echo "Getting source maps"

mkdir -p "$OUTPUT_DIR"

while IFS= read -r -d '' file
do
  MODULE="$(basename "$file" .purs)"
  echo "Copying files for $MODULE"
  mkdir -p "$OUTPUT_DIR/$MODULE"
  cp -r \
    "$TEST_MODULES_DIR/SourceMaps.$MODULE/index.js" \
    "$TEST_MODULES_DIR/SourceMaps.$MODULE/index.js.map" \
    "$OUTPUT_DIR/$MODULE/"
  cp "$file" "$OUTPUT_DIR/$MODULE/$MODULE.purs"
done <   <(find "tests/purs/sourcemaps" -type f -wholename '*.purs' -print0)
