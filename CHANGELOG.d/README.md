This directory contains changelog entries for work that has not yet been
released. When a release goes out, these files will be concatenated and
prepended to CHANGELOG.md in a new section for that release.

Maintainers: see update-changelog.hs for details of this process.

Contributors: read on!

When you are preparing a new PR, add a new file to this directory. The file
should be named `{PREFIX}_{SLUG}.md`, where `{PREFIX}` is one of the following:
* `breaking`: for breaking changes
* `feature`: for new features
* `fix`: for bug fixes
* `internal`: for work that will not directly affect users of PureScript
* `misc`: for anything else that needs to be logged

`{SLUG}` should be a short description of the work you've done. The name has no
impact on the final CHANGELOG.md.

Some example names:
* `fix_issue-9876.md`
* `breaking_deprecate-classes.md`
* `misc_add-forum-to-readme.md`

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
