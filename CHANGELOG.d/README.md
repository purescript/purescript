This directory contains changelog entries for work that has not yet been
released. When a release goes out, these files will be concatenated and
prepended to CHANGELOG.md in a new section for that release.

Maintainers: see update-changelog.hs for details of this process.

Contributors: read on!

Our guiding principle is that the changelog is a tool for users—people who
depend on PureScript as a compiler or as a library—who are considering
upgrading, or have recently upgraded, their PureScript compiler version. We ask
that when making changes that such users might need to know about, you help
them out by adding to our changelog.

Work that doesn't change the compiler (such as updates to README.md) doesn't
need a changelog entry. But keep in mind that even parts of the project like
our CI workflow can introduce changes to the compiler we release.

When you are preparing a new PR that does change the compiler, add a new file
to this directory. The file should be named `{PREFIX}_{SLUG}.md`, where
`{PREFIX}` is one of the following:
* `breaking`: for breaking changes to the compiler, for which a user may need to do
  work to their project before or immediately upon upgrading
* `feature`: for new features, which might prevent a user from downgrading to an
  earlier version
* `fix`: for bug fixes, which might motivate a user to upgrade
* `internal`: for work that is not expected to directly affect users; these
  entries should usually be brief, but may serve as useful starting points for
  investigations if a change ends up having unintended consequences

(There is also a fifth prefix, `misc`. This is an escape hatch in case we have
something that somehow doesn't fit in the above categories but that we want to
include in the changelog, which frankly seems unlikely given how much of a
catch-all `internal` is. We'll tell you if you should use this one.)

`{SLUG}` should be a short description of the work you've done. The name has no
impact on the final CHANGELOG.md.

Some example names:
* `fix_issue-9876.md`
* `breaking_deprecate-classes.md`
* `internal_use-ubuntu-38.04-in-ci.md`

The contents of the file can be as brief as:

```markdown
* A short message, like the title of your commit
```

Please remember the initial `*`! These files will all be concatenated into
lists.

If you have more to say about your work, indent additional lines like so:

``````markdown
* A short message, like the title of your commit

  Here is a longer explanation of what this is all about. Of course, this file
  is Markdown, so feel free to use *formatting*

  ```
  and code blocks
  ```

  if it makes your work more understandable.
``````

You do not have to edit your changelog file to include a reference to your PR.
The CHANGELOG.md updating script will do this automatically and credit you.
