#!/bin/bash

set -e

force_recompile='false'
force_reinstall='false'

while getopts 'ci' flag; do
  case "${flag}" in
    c) force_recompile='true' ;;
    i) force_reinstall='true' ;;
    *) error "Unexpected option ${flag}" ;;
  esac
done

if [ "$force_reinstall" = "true" ] && [ -d "bower_components" ]; then
  echo "Reinstalling core packages..."
  rm -r bower_components
fi

npm install bower

node_modules/.bin/bower i \
        purescript-prelude \
        purescript-eff \
        purescript-st \
        purescript-integers \
        purescript-functions \
        purescript-console \
        purescript-profunctor \
        purescript-contravariant \
        purescript-parallel \
        purescript-control \
        purescript-tailrec \
        purescript-maps \
        purescript-free \
        purescript-transformers \
        purescript-exists \
        purescript-monoid \
        purescript-either \
        purescript-maybe \
        purescript-inject \
        purescript-graphs \
        purescript-enums \
        purescript-unfoldable \
        purescript-coproducts \
        purescript-lazy \
        purescript-distributive \
        purescript-identity \
        purescript-bifunctors \
        purescript-const \
        purescript-sets \
        purescript-quickcheck \
        purescript-foreign \
        purescript-foldable-traversable \
        purescript-tuples \
        purescript-strings \
        purescript-arrays \
        purescript-random \
        purescript-refs \
        purescript-globals \
        purescript-exceptions \
        purescript-validation \
        purescript-parallel \
        purescript-proxy \
        purescript-semirings \
        purescript-math \
        purescript-generics

if [ "$force_recompile" = "true" ] && [ -d "output" ]; then
  echo "Recompiling..."
  rm -r output
fi

../dist/build/psc/psc tests/*/*.purs \
                      'bower_components/purescript-*/src/**/*.purs' \
                --ffi 'bower_components/purescript-*/src/**/*.js'
