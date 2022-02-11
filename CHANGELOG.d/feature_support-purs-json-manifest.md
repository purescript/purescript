- Add support for publishing via the `.purs.json` manifest format

  This feature expands compiler support for publishing packages with different
  manifest formats. Previously, packages had to have a `bower.json` manifest;
  now, packages can choose to have a `.purs.json` manifest instead.

  This feature provides only partial support for packages published to the
  PureScript registry using the `.purs.json` manifest format. Registry packages
  are allowed to be hosted anywhere (not just GitHub), and do not need to be
  Git repositories at all. However, `purs publish` and its primary consumer,
  Pursuit, both require packages to be available on GitHub and for their version
  to be a SemVer-compliant Git tag. Therefore, this feature only supports
  registry packages that are compatible with these restrictions.
