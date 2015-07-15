set -e

configure_flags="--enable-tests -v2"

if ! git describe --tags --exact-match >/dev/null 2>/dev/null
then
  # Not a release build
  configure_flags="--disable-optimization $configure_flags"
fi

if [ -n "$COVERAGE_SUITE" ]
then
  configure_flags="--enable-coverage $configure_flags"
fi

echo "> cabal configure $configure_flags"
cabal configure $configure_flags
