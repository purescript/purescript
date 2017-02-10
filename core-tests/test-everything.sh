#!/usr/bin/env bash

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

# todo : fix this once core libraries reach 1.0
yes 1 | bower i

if [ "$force_recompile" = "true" ] && [ -d "output" ]; then
  echo "Recompiling..."
  rm -r output
fi

stack exec purs compile 'tests/**/*.purs' 'bower_components/purescript-*/src/**/*.purs'

stack exec purs docs 'bower_components/purescript-*/src/**/*.purs' > core-docs.md

NODE_PATH=output node -e "require('Test.Main').main()"
