# Versioning Policy

PureScript can be perceived from two different perspectives:
1. PureScript-the-application (e.g. using `purs` to compile code)
1. PureScript-the-library (e.g. building a tool that depends on the [`purescript` package](https://hackage.haskell.org/package/purescript))

This project is versioned using [SemVer 2.0.0](https://semver.org/), not [PVP](https://pvp.haskell.org/) because users of PureScript-the-application are the intended audience. Thus, breaking changes to PureScript-the-application are reflected in this project's version. Since the compiler's artifacts (e.g. the externs format, `CoreFn`) are considered part of PureScript-the-application, a breaking change to these things is reflected in the project version.

Since PureScript-the-library is used by internal tools like [Try PureScript](https://github.com/purescript/trypurescript) and [Pursuit](https://github.com/purescript/pursuit), it must be published to Hackage as a library. However, PureScript-the-library is considered unstable and can make breaking changes to library users without reflecting that in the version.
