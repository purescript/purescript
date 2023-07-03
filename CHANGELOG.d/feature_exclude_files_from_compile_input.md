* Exclude files from compiler input

  The compiler now supports excluding files from the globs given to it as input.
  This means there's now a new option for `purs compile`, namely
  `--exclude-files` (or the short version `-x`):

```sh
> purs compile --help
Usage: purs compile [FILE] [-x|--exclude-files ARG] [-o|--output ARG] ...

  Compile PureScript source files

Available options:
  -h,--help                Show this help text
  FILE                     The input .purs file(s).
  -x,--exclude-files ARG   Glob of .purs files to exclude from the supplied
                           files.
  ...
```

This allows you to keep related files closer together (that is, [colocate](https://kentcdodds.com/blog/colocation) them).

Consider a setup like the following:

```sh
src/
    Main.purs
    View/
        LoginPage.purs
        LoginPageTest.purs
        LoginPageStories.purs
```

In order to exclude the files in the example above you can now invoke `purs`
like this and it will only compile `LoginPage.purs`:

```sh
purs compile "src/**/*.purs" --exclude-files "src/**/*Stories.purs" -x "src/**/*Test.purs"
```

With `spago`, the equivalent command is:

```sh
spago build --purs-args '-x "src/**/*Test.purs" -x "src/**/*Stories.purs"'
```
