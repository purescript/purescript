# Release Checklist

## For every release

- [ ] Regenerate LICENSE (see `license-generator/`)
- [ ] Release notes
- [ ] Publish to Hackage
- [ ] Update npm package
- [ ] Try PureScript? &mdash; need to decide whether we want to continue
      officially supporting this

## Libraries

Are there breaking changes to the language? Or alternatively, are there
language changes which require breaking changes in the relevant libraries to
make use of? If so:

- [ ] Update core libraries
- [ ] Update contrib libraries
- [ ] Update node bindings
- [ ] Create a new package set

## Tools

Has the compiler CLI changed at all? If so, the following may need updates:

- [ ] psc-package
- [ ] Pulp
- [ ] purs-loader
- [ ] ide plugins

## JSON formats

Have any of the following JSON formats changed? If so, it may be worth
considering what effects this may have:

- [ ] Corefn
- [ ] Ide protocol
- [ ] JSON produced by `purs publish`
  - [ ] check whether this affects Pursuit

## Documentation

- [ ] Check that purescript.org is up-to-date
- [ ] Check that INSTALL.md is up-to-date

Have there been any changes or additions to the language which should be
documented?

- [ ] Document any language changes in the documentation repo

Have there been additions or changes to `Prim` (including documentation
changes?) If so,

- [ ] Update Pursuit to depend on the latest compiler so that these docs appear
      on pursuit.purescript.org

## Announcements

- [ ] Discourse
- [ ] Twitter
- [ ] /r/purescript
