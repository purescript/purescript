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

if [ "$force_reinstall" = "true" ] && [ -d ".psc-package" ]; then
  echo "Reinstalling core packages..."
  rm -rf .psc-package
fi

psc-package update

if [ "$force_recompile" = "true" ] && [ -d "output" ]; then
  echo "Recompiling..."
  rm -r output
fi

stack exec psc-package build

stack exec purs docs $(psc-package sources) > core-docs.md

NODE_PATH=output node -e "require('Test.Main').main()"
