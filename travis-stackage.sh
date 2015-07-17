set -e

if [ -n "$STACKAGE" ]
then
  curl -LO https://www.stackage.org/$STACKAGE/cabal.config
  head -n 1 cabal.config
fi
