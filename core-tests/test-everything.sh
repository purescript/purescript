#!/bin/bash

set -e

npm install -g bower

bower i purescript-profunctor \
        purescript-contravariant \
        purescript-parallel \
        purescript-control \
        purescript-tailrec \
        purescript-maps \
        purescript-free \
        purescript-transformers \
        purescript-monad-eff \
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
        purescript-math
  
find bower_components/**/src -name '*.purs' | xargs ../dist/build/psc-make/psc-make
