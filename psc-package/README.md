# `psc-package`

`psc-package` is an executable which helps manage PureScript dependencies via Git. It can be used directly, but it is also designed to be used by external tools.

## Concepts

### Package Sets

A _package set_ is a mapping from package names to:

- the Git repository URL for the package
- the Git ref which should be passed to `git clone` to clone the appropriate version (usually a tag name, but a SHA is also valid)
- the package's transitive dependencies

A package set repository contains a `packages.json` file which contains all mapping information. `psc-package` uses this information to decide which repos need to be cloned.

The default package set is [purescript/package-sets](https://github.com/purescript/package-sets), but it is possible to create custom package sets by forking an existing package set or creating a new one from scratch. One benefit of using the default package set is that it is verified by a continuous integration process.

## The `psc-package.json` format

Here is a simple project configuration:

```json
{
    "name": "my-project",
    "set": "psc-0.10.2",
    "source": "https://github.com/purescript/package-sets.git",
    "depends": [
        "prelude"
    ]
}
```

It defines:

- The project name
- The package set to use to resolve dependencies (this corresponds to a branch or tag of the package set source repository)
- The package set source repository Git URL (change this if you want to host your own package sets)
- Any dependencies of the project, as a list of names of packages from the package set

## How To

### Create a project

A new package can be created using `psc-package init`. This will:

- Create a simple `psc-package.json` file based on the current compiler version
- Add the Prelude as a dependency (this can be removed later)
- Sync the local package database (under the `.psc-package/` directory) by cloning any necessary repositories.

### Add dependencies

To add a dependency, either:

- Use the `install` command, which will update the project configuration automatically, or
- Modify the `psc-package.json` file, and sync manually by running the `update` command.

### Build a project

Active project dependencies and project source files under `src` can be compiled using the `build` command.

This command is provided as a convenience until external tools add support for `psc-package`. It _might_ be removed in future.

### Query the local package database 

The local package database can be queried using the following commands:

- `sources` - list source directories for active package versions. This can be useful when building a command for, say, running PSCi.
- `dependencies` - list all transitive dependencies

### Add a package to the package set

Adding your package to the package set means that others can easily install it as a dependency.

Please note that your package will be removed from the set if it is not kept up to date. It can be easily re-added later if this happens.

Adding a package is a manual process right now. We would like to add commands to make this process simpler, but for now, please follow these steps:

- Tag a release of your library
- Run the `dependencies` command to get the list of transitive dependencies
- Make a pull request on the package set repository (against `master`) to add a new entry to `packages.json`. Use the dependency information above to fill in the fields, and the name of your new tag.

Travis will verify your package builds correctly, and then we will try to merge your pull request. Your package will then be available in the next tagged package set.
 
### Update a package in the set

- Tag a new release
- Make a pull request on `master` to modify the tag named in the package set repository.

Again, once Travis verifies your change, we will merge it into `master` and your change will be available in the next tag.
 
## FAQ

### Can I add a dependency which is not in the package set?

Not right now. We might add this feature in future, but for now, consider either:

- Adding your dependency to the package set if possible, or
- Creating your own custom package set
