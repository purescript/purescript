This error occurs when the type checker knows a token refers to a module, but is unable to find a definition for that module. For example,

```purs
import MyModule
```

yields

```
Unknown module MyModule
```

if there is no accompanying definition of `MyModule` in any loaded files.

If in PSCI, a possible fix is to load the file which has a definition for the module:

```purs
> :load src/MyProject.purs
> import MyModule
```

where `src/MyProject.purs` includes a definition for `MyModule`.
