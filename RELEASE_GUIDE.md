# Release Guide (for maintainers)

## Before making a release

- Check that there are no unintended breaking changes by compiling [the latest package set](https://github.com/purescript/package-sets/releases/latest)
- Check that INSTALL.md is up-to-date
- Regenerate LICENSE: `make license-generator` (see `license-generator/` for
  details)
- Write release notes

Additionally, if there are any breaking changes, there are number of downstream
projects who we should probably at least notify:

### Libraries

Are there breaking changes to the language? Or alternatively, are there
language changes which require breaking changes in the relevant libraries to
make use of? If so:

- Update core libraries
- Update contrib libraries
- Update node bindings
- Update web bindings

### Tools

Has the compiler CLI changed at all? If so, the following may need updates:

- spago
- pulp
- psc-package
- purs-loader
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
        that the new version should be based on the PVP, according to what
        changed since the previous release, and not on the actual compiler
        version)

      - The versions table in `lib/purescript-cst/README.md`,

      - The version bounds for `purescript-cst` in `purescript.cabal`

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

## After making a release

- Document any language changes in the documentation repo
  - In particular, it's worth checking that the getting started guide in the
    documentation repo still works
- If there have been changes to any `Prim` modules (even if they are just
  documentation changes), update Pursuit to depend on the latest compiler so
  that these docs appear on pursuit.purescript.org
- Update Try PureScript
- Make release announcements:
  - Discourse
  - Twitter
  - /r/purescript
