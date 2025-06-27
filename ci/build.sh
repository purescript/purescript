#!/bin/bash

set -ex

# Provides expanders that group console output in GitHub Actions
# See https://docs.github.com/en/actions/reference/workflow-commands-for-github-actions#grouping-log-lines
(echo "::group::Initialize variables") 2>/dev/null

# This is the main CI build script. It is intended to run on all platforms we
# run CI on: linux, mac os, and windows. It makes use of the following
# environment variables:
#
# - CI_RELEASE
#
#   If set to "true", passes the RELEASE flag to the compiler, and enables
#   optimizations. Otherwise, we disable optimizations (to speed builds up).
#
# = Source distributions
#
# During a normal build, we create a source distribution with `stack sdist`,
# and then compile and run tests inside that. The reason for this is that it
# helps catch issues arising from forgetting to list files which are necessary
# for compilation or for tests in our package.yaml file (these sorts of issues
# don't test to get noticed until after releasing otherwise).

# We test with --haddock because haddock generation can fail if there is invalid doc-comment syntax,
# and these failures are very easy to miss otherwise.
STACK="stack --no-terminal --haddock --jobs=4"

STACK_OPTS="--test"
if [ "$CI_RELEASE" = "true" -o "$CI_PRERELEASE" = "true" ]
then
  STACK_OPTS="$STACK_OPTS --flag=purescript:RELEASE"
else
  STACK_OPTS="$STACK_OPTS --fast"
fi
if [ "$CI_STATIC" = "true" ]
then
  STACK_OPTS="$STACK_OPTS --flag=purescript:static"
fi

(echo "::endgroup::"; echo "::group::Set version number for build") 2>/dev/null

if [ "$CI_PRERELEASE" = "true" ]
then
  git fetch --depth=1 origin "v$(npm view purescript@next version)"

  # List of files/folders to use to detect if a new prerelease should be
  # issued. Any path that could contain files that affect the built bundles or
  # the published npm package should be included here. Paths that no longer
  # exist should be deleted. A false positive is not as big a deal as a false
  # negative, so err on the side of including stuff.
  if git diff --quiet FETCH_HEAD HEAD -- \
    .github/workflows app bundle ci npm-package src \
    purescript.cabal stack.yaml
  then
    echo "Skipping prerelease because no input affecting the published package was"
    echo "changed since the last prerelease"
    echo "do-not-prerelease=true" >> $GITHUB_OUTPUT
  else
    do_prerelease=true
  fi
fi

package_version=$(node -pe 'require("./npm-package/package.json").version')
package_release_version=${package_version%%-*}
package_prerelease_suffix=${package_version#$package_release_version}

if ! grep -q "\"install-purescript --purs-ver=${package_version//./\\.}\"" npm-package/package.json
then
  echo "Version in npm-package/package.json doesn't match version in install-purescript call"
  exit 1
fi

if ! grep -q "^version:\\s*${package_release_version//./\\.}$" purescript.cabal
then
  echo "Version in npm-package/package.json doesn't match version in purescript.cabal"
  exit 1
fi

if ! grep -q "^prerelease = \"${package_prerelease_suffix//./\\.}\"$" app/Version.hs
then
  echo "Version in npm-package/package.json doesn't match prerelease in app/Version.hs"
  exit 1
fi

if [ "$do_prerelease" ]
then
  # (some versions of?) macOS have an old FreeBSD sed that requires -i to be followed with an argument
  if sed --version >/dev/null
  then
    # Probably GNU sed
    sedi=(sed -i)
  else
    # Probably FreeBSD sed
    sedi=(sed -i '')
  fi

  function largest-matching-git-tag {
    grep -E "^${1//./\\.}(\\.|$)" "$git_tags" | head -n 1
  }

  git_tags=$(mktemp)
  trap 'rm "$git_tags"' EXIT
  git ls-remote --tags -q --sort=-version:refname | sed 's_^.*refs/tags/__' > $git_tags

  pushd npm-package

  if [ "$package_prerelease_suffix" ]
  then
    tag=$(largest-matching-git-tag "v$package_release_version${package_prerelease_suffix%%.*}")
    if [ "$tag" ]
    then
      npm version --allow-same-version "$tag"
      build_version=$(npm version --no-git-tag-version prerelease)
      build_version=${build_version#v}
    else
      build_version=$package_version
    fi
  else # (current version does not contain a prerelease suffix)
    if grep -Fqx "v$package_release_version" "$git_tags"
    then # (the current version has been published)
      bump=patch
      if [ "$(find ../CHANGELOG.d -maxdepth 1 -name 'breaking_*' -print -quit)" ]
      then
        # If we ever reach 1.0, change this to major and uncomment the below
        bump=minor
      #elif [ "$(find ../CHANGELOG.d -maxdepth 1 -name 'feature_*' -print -quit)" ]
      #then
      #  bump=minor
      fi
      next_tag=$(npm version --no-git-tag-version "$bump")
      tag=$(largest-matching-git-tag "$next_tag-[0-9]+")
      if [ "$tag" ]
      then
        npm version --allow-same-version "$tag"
        build_version=$(npm version --no-git-tag-version prerelease)
      else
        build_version=$(npm version --allow-same-version "$next_tag-0")
      fi
      build_version=${build_version#v}
    else # (current version has not been published)
      build_version=$package_version
      echo "do-not-prerelease=true" >> $GITHUB_OUTPUT
    fi
  fi

  echo "version=$build_version" >> $GITHUB_OUTPUT

  popd

  if [ "$build_version" != "$package_version" ]
  then
    build_release_version=${build_version%%-*}
    build_prerelease_suffix=${build_version#$build_release_version}
    # We don't need to update the install-purescript command before we build;
    # we'll do that when we publish. All we need to update here are the files
    # that affect the purs binary.
    "${sedi[@]}" -e "s/^\\(version:[[:blank:]]*\\)${package_release_version//./\\.}/\1$build_release_version/" purescript.cabal
    "${sedi[@]}" -e "s/^prerelease = \"${package_prerelease_suffix//./\\.}\"$/prerelease = \"${build_prerelease_suffix}\"/" app/Version.hs
  fi
fi

(echo "::endgroup::"; echo "::group::Install snapshot dependencies") 2>/dev/null

# Install snapshot dependencies (since these will be cached globally and thus
# can be reused during the sdist build step)
$STACK build --only-snapshot $STACK_OPTS

(echo "::endgroup::"; echo "::group::Build source distributions") 2>/dev/null

# Test in a source distribution (see above)
$STACK sdist . --tar-dir sdist-test;
tar -xzf sdist-test/purescript-*.tar.gz -C sdist-test --strip-components=1

(echo "::endgroup::"; echo "::group::Build and test PureScript") 2>/dev/null

pushd sdist-test
# Haddock -Werror goes here to keep us honest but prevent failing on
# documentation errors in dependencies
$STACK build $STACK_OPTS --haddock-arguments --optghc=-Werror

if [ "$do_prerelease" ]
then
  if [ "$($STACK exec -- purs --version)" != "$build_version" ]
  then
    echo "purs --version doesn't equal the expected value"
    exit 1
  fi
fi
popd

(echo "::endgroup::") 2>/dev/null
