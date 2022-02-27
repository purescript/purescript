# Release Guide (for maintainers)

## Prerequisites

- You will need a [Hackage](https://hackage.haskell.org/) account that has been invited to be a maintainer of the `purescript` package on Hackage. If you don't have one, create one and ask to be invited as a maintainer.
- You will need an [NPM](https://www.npmjs.com/) account that has been invited to be a maintainer of the `purescript` package on NPM. If you don't have one, create one and ask to be invited as a maintainer.
- You need `spago` installed.
- You need to be logged into NPM (i.e. running `npm whoami` should print your NPM account's username)

## Before making a release

- Check that there are no unintended breaking changes by compiling [the latest package set](https://github.com/purescript/package-sets/releases/latest)

```bash
stack build
mkdir wPackageSet
pushd wPackageSet
spago init
spago upgrade-set
# install all packages in the set
spago install $(spago ls packages | cut -f 1 -d ' ' | tr '\n' ' ')

# Verify that code compiles and docs are properly created
stack exec bash <<EOF
spago build
spago docs -S
EOF
popd
# rm -rf wPackageSet
```

- Check that INSTALL.md is up-to-date. For example:
    - Does the GHC version correspond to the one we're currently on?
    - Is the list of supported OSes (based on GHC version) correct? If not, check the GHC website for the list.
    - Are there any unofficial installation methods that should be dropped because they are no longer maintained?
    - Are the instructions for building from source currently working?
    - Anything else that needs to be changed?

- Regenerate LICENSE: `make license-generator` (see `license-generator/` for details)

- Additionally, if there are any breaking changes, there are number of downstream
projects whom we should probably at least notify. See below subsections:

### Libraries

Are there breaking changes to the language? Alternatively, are there
language changes which require breaking changes in the relevant libraries to
make use of? If so:

- Update core libraries
- Update contrib libraries
- Update node bindings
- Update web bindings

### Tools

Has the compiler CLI changed at all? If so, the following may need updates:

- [spago](https://github.com/purescript/spago)
- [pulp](https://github.com/purescript-contrib/pulp)
- [psc-package](https://github.com/purescript/psc-package)
- [purs-loader](https://github.com/ethul/purs-loader)
- ide plugins

### JSON formats

Have any of the following JSON formats changed? If so, it may be worth
considering what effects this may have:

- Corefn
- Ide protocol
- JSON produced by `purs publish`
  - this might affect Pursuit

## Making a release

- Make a commit bumping versions. The following should be updated:

  - The `version` field in `purescript.cabal`

  - The `prerelease` field in `app/Version.hs`, if updating the prerelease
    field

  - The `version` field in `npm-package/package.json`

  - The version to install in the `postinstall` script in `package.json`

  - If `purescript-cst` has changed at all since the last release:

      - The `version` field in `lib/purescript-cst/purescript-cst.cabal` (note
        that the new version should be based on the [PVP](https://pvp.haskell.org/),
        according to what changed since the previous release, and not on the actual compiler version)

      - The versions table in `lib/purescript-cst/README.md`,

      - The version bounds for `purescript-cst` in `purescript.cabal`

- Run `stack update-changelog.hs`, which will move the entries in `CHANGELOG.d`
  to a new section in `CHANGELOG.md` labeled with the new version.

- Submit a PR with the above commits and get it merged.

- Create a release from the releases tab in GitHub and copy in the release
  notes. This will also create a tag, which will kick off a CI build, which
  will upload prebuilt compiler binaries to the release on GitHub when it
  completes. (If the CI build fails, binaries can also be built locally and
  manually uploaded to the release on GitHub)

- Publish to Hackage:

  - change to the `lib/purescript-cst` directory and run `stack upload .`

  - Finally, run `stack upload .` from the repo root directory.

  It's a good idea to check that the two packages (`purescript` and
  `purescript-cst`) can be installed from Hackage at this
  point.

- After all of the prebuilt binaries are present on the GitHub releases page,
  publish to npm: change to the `npm-package` directory and run `npm publish`.
  It's a good idea to check that the package can be installed from npm at this
  point.

Note: if a release does not go as planned (e.g. [`v0.14.3`](https://github.com/purescript/purescript/pull/4139)), we should not delete the broken GitHub release or its Git tag. Rather, we should make a new release and update the GitHub release notes and the corresponding section in the CHANGELOG.md file for the broken release to
1. say that it's not a real release, and
1. refer people to the newer release.

## After making a release

- Document any language changes in the documentation repo
  - In particular, it's worth checking that the getting started guide in the
    documentation repo still works
- If any of the below conditions are true, update Pursuit to depend on the latest release
  and redeploy it:
    - there have been changes to any `Prim` modules (even if they are just
  documentation changes)
    - there have been changes to the documentation JSON format
- Create a new package set for the release
- Update Try PureScript to the latest release and package set and redeploy it
- Make release announcements:
  - Discourse
  - Discord
  - Twitter
  - /r/purescript
