# Protocol

Communication with `purs ide server` is via a JSON protocol over a TCP connection:
the server listens on a particular (configurable) port, and will accept a single line
of JSON input in the format described below, terminated by a newline, before giving 
a JSON response and closing the connection.

The `purs ide client` command can be used as a wrapper for the TCP connection, but
otherwise behaves the same, accepting a line of JSON on stdin and exiting after
giving a result on stdout.

The result needs to be unwrapped from the "wrapper" which separates success
from failure. This wrapper is described at the end of this document.

## Command:
### Load
The `load` command "loads" the requested modules into the server for completion
and type info. If the `params` object is left off, the `load` command will try
to detect all the compiled modules in your project and load them.

**Params:**
 - `modules :: (optional) [ModuleName]`: A list of modules to load.
 psc-ide-server will try to parse all the declarations in these modules

```json
{
  "command": "load",
  "params": (optional) {
    "modules": (optional)["Module.Name1", "Module.Name2"]
  }
}
```

**Result:**

The Load Command returns a string with a summary about the loading process.

### Type
The `type` command looks up the type for a given identifier. It also returns the
definition position, if it can be found in the passed source files.

**Params:**
 - `search :: String`: The identifier to look for. Only matches on equality.
 - `filters :: (optional) [Filter]`: These filters will be applied before looking for the
  identifier. These filters get combined with *AND*, so a candidate must match *ALL*
  of them to be eligible.
 - `currentModule :: (optional) String`: see *Complete* command
```json
{
  "command": "type",
  "params": {
    "search": "filterM",
    "filters": [{..}],
    "currentModule": "Main"
  }
}
```

**Result:**
The possible types are returned in the same format as completions

### Complete
The `complete` command looks up possible completions/corrections.

**Params**:
 - `filters :: [Filter]`: The same as for the `type` command. A candidate must
  match all filters.

 - `matcher :: (optional) Matcher`: The strategy used for matching candidates
  after filtering. Results are scored internally and will be returned in the
  descending order where the nth element is better then the n+1-th.
  If no matcher is given every candidate, that passes the filters, is returned
  in no particular order.

 - `currentModule :: (optional) String`: The current modules name. If it matches
   with the rebuild cache non-exported modules will also be completed. You can
   fill the rebuild cache by using the "Rebuild" command.

 - `options :: (optional) CompletionOptions`: The CompletionOptions to apply to
   the completion results

```json
{
  "command": "complete",
  "params": {
    "filters": [{..}, {..}],
    "matcher": {..},
    "currentModule": "Main",
    "options": {
      "maxResults": 50,
      "groupReexports": true
    }
  }
}
```

**Result:**

The following format is returned as the Result:

Both the `definedAt` aswell as the `documentation` field might be `null` if they
couldn't be extracted from a source file.

```json
[
  {
  "module": "Data.Array",
  "identifier": "filter",
  "type": "forall a. (a -> Boolean) -> Array a -> Array a",
  "expandedType": "forall a. (a -> Boolean) -> Array a -> Array a",
  "definedAt":
    {
    "name": "/path/to/file",
    "start": [1, 3],
    "end": [3, 1]
    },
  "documentation": "A filtering function",
  "exportedFrom": ["Data.Array"]
  }
]
```


### CaseSplit

The CaseSplit command takes a line of source code, an area in that line of code
and replaces it with all patterns for a given type. The parameter `annotations`
is used to turn type annotations on or off for the constructor fields.

```json
{
 "command": "caseSplit",
 "params": {
  "line": "elem a as",
  "begin": 8,
  "end": 10,
  "annotations": true,
  "type": "List"
 }
}
```

**Result:**

The following format is returned as the Result:

```json
[
  "elem a Nil",
  "elem a (Cons (_ :: a) (_ :: List a))"
]
```
You should then be able to replace the affected line of code in the editor with the new suggestions.

### Add Clause

The AddClause command takes a typedeclaration and generates a function template for the given type.
The `annotations` option turns type annotations on or off for the function arguments.

```json
{
 "command": "addClause",
 "params": {
  "line": "elem :: forall a. (Eq a) => a -> List a",
  "annotations": true
 }
}
```

**Result:**

The following format is returned as the Result:

```json
[
  "elem :: forall a. (Eq a) => a -> List a",
  "elem ( _ :: a) = ?elem"
]
```
You should then be able to replace the affected line of code in the editor with the new suggestions.

### Import

For now all of the import related commands work with a file on the filesystem.

You can specify it with the `file` parameter.

If you supply the optional `outfile` parameter, the output will be written to
that file, and an info message will be returned from the client.

If you don't supply `outfile`, the server responds with a list of strings which,
when inserted into a file linewise create the module with the applied changes.

Arguments:

- `file` :: String
- `outfile` :: Maybe String
- `filters` :: Maybe [Filter]

Example:

```json
{
  "command": "import",
  "params": {
    "file": "/home/creek/Documents/chromacannon/src/Main.purs",
    "outfile": "/home/creek/Documents/chromacannon/src/Main.purs",
    "filters": [{
      "filter": "modules",
      "params": {
        "modules": ["My.Module"]
      }
    }],
    "importCommand": {
      "yadda": "yadda"
    }
  }
}
```


#### Subcommand `addImplicitImport`

This command just adds an unqualified import for the given modulename.

Arguments:
- `module :: String`

Example:
```json
{
  "command": "import",
  "params": {
    "file": "/home/creek/Documents/chromacannon/src/Main.purs",
    "importCommand": {
      "importCommand": "addImplicitImport",
      "module": "Data.Array.LOL"
    }
  }
}
```

#### Subcommand `addQualifiedImport`

This command adds an import for the given modulename and qualifier.

Arguments:
- `module :: String`
- `qualifier :: String`

Example:
```json
{
  "command": "import",
  "params": {
    "file": "/home/creek/Documents/chromacannon/src/Main.purs",
    "importCommand": {
      "importCommand": "addQualifiedImport",
      "module": "Data.Array",
      "qualifier": "Array"
    }
  }
}
```

#### Subcommand `addImport`

This command takes an identifier and searches the currently loaded modules for
it. If it finds no matches it responds with an Error. If it finds exactly one
match it adds the import and returns. If it finds more than one match it
responds with a list of the found matches as completions like the complete
command.

You can also supply a list of filters like the ones for completion. These are
specified as part of the top level command rather than within the `importCommand`.
This way you can narrow down the search to a certain module and resolve the case in which
more then one match was found.

Arguments:
- `identifier :: String`
- `qualifier :: String` (optional)

Example:
```json
{
  "command": "import",
  "params": {
    "file": "/home/creek/Documents/chromacannon/src/Demo.purs",
    "outfile": "/home/creek/Documents/chromacannon/src/Demo.purs",
    "importCommand": {
      "importCommand": "addImport",
      "identifier": "bind"
    }
  }
}
```

Example with qualifier and filter:
```json
{
  "command": "import",
  "params": {
    "file": "/home/creek/Documents/chromacannon/src/Demo.purs",
    "outfile": "/home/creek/Documents/chromacannon/src/Demo.purs",
    "importCommand": {
      "importCommand": "addImport",
      "identifier": "length",
      "qualifier": "Array"
    },
    "filters": [{
      "filter": "modules",
      "params": {
        "modules": ["Data.Array"]
      }
    }]
  }
}
```

### Rebuild

The `rebuild` command provides a fast rebuild for a single module. It doesn't
recompile the entire project though. All the modules dependencies need to be
loaded. A successful rebuild will be stored to allow for completions of private
identifiers.

Arguments:
  - `file :: String` the path to the module to rebuild
  - `actualFile :: Maybe String` Specifies the path to be used for location
    information and parse errors. This is useful in case a temp file is used as
    the source for a rebuild.

```json
{
  "command": "rebuild",
  "params": {
    "file": "/path/to/file.purs"
    "actualFile": "/path/to/actualFile.purs"
  }
}
```

**Result**

In the Success case you get a list of warnings in the compilers json format.

In the Error case you get the errors in the compilers json format

### Pursuit
The `pursuit` command looks up the packages/completions for a given identifier from Pursuit.

**Params:**
 - `query :: String`: With `type: "package"` this should be a module name. With
   `type: "completion"` this can be any string to look up.
 - `type :: String`: Takes the following values:
   - `package`: Looks for packages that contain the given module name.
   - `completion` Looks for declarations for the query from Pursuit.

```json
{
  "command": "pursuit",
  "params": {
    "query": "Data.Array",
    "type": "package"
  }
}
```

**Result:**

`package` returns:

```json
[
  {
  "module": "Module1.Name",
  "package": "purescript-packagename"
  }
]
```

`completion` returns:

```json
[
  {
  "module": "Data.Array",
  "identifier": "filter",
  "type": "forall a. (a -> Boolean) -> Array a -> Array a",
  "package": "purescript-arrays"
  }
]
```

### List

#### Loaded Modules

`list` of type `loadedModules` lists all loaded modules (This means they can be searched for completions etc)

```json
{
  "command": "list",
  "params": {
    "type": "loadedModules"
  }
}
```

#### Response:

The list loadedModules command returns a list of strings.

#### Available Modules

`list` of type `availableModules` lists all available modules. (This basically
means the contents of the `output/` folder.))

```json
{
  "command": "list",
  "params": {
    "type": "availableModules"
  }
}
```

#### Response:

The list availableModules command returns a list of strings.

#### Imports

The list commmand can also list the imports for a given file.

```json
{
  "command": "list",
  "params": {
    "type": "import",
    "file": "/home/kritzcreek/Documents/psc-ide/examples/Main.purs"
  }
}
```

#### Response:

The list import command returns the parse module name as well as a list of
imports like so:

```json

{
  "moduleName": "MyModule",
  "imports": [Import]
}

The different kind of imports are returned like so:

```

Implicit Import (`import Data.Array`):
```json
{
  "module": "Data.Array",
  "importType": "implicit"
}
```

Implicit qualified Import (`import Data.Array as A`):
```json
{
  "module": "Data.Array",
  "importType": "implicit",
  "qualifier": "A"
}
```

Explicit Import (`import Data.Array (filter, filterM, join)`):
```json
{
  "module": "Data.Array",
  "importType": "explicit",
  "identifiers": ["filter", "filterM", "join"]
}
```

Explicit qualified Import (`import Data.Array (filter, filterM, join) as A`):
```json
{
  "module": "Data.Array",
  "importType": "explicit",
  "identifiers": ["filter", "filterM", "join"],
  "qualifier": "A"
}
```

Hiding Import (`import Data.Array hiding (filter, filterM, join)`):
```json
{
  "module": "Data.Array",
  "importType": "hiding",
  "identifiers": ["filter", "filterM", "join"]
}
```

Qualified Hiding Import (`import Data.Array hiding (filter, filterM, join) as A`):
```json
{
  "module": "Data.Array",
  "importType": "hiding",
  "identifiers": ["filter", "filterM", "join"],
  "qualifier": "A"
}
```

### Cwd/Quit/Reset
`cwd` returns the working directory of the server(should be your project root).

`quit` quits the server.

`reset` resets all loaded modules.

```json
{
  "command": "cwd|quit|reset"
}
```

**Result:**
These commands return strings.

## Filter:

### Exact filter
The Exact filter only keeps identifiers that are equal to the search term.

```json
{
  "filter": "exact",
  "params": {
    "search": "filterM"
  }
}
```
### Prefix filter
The Prefix filter keeps identifiers/modules/data declarations that
are prefixed by the search term.

```json
{
   "filter": "prefix",
   "params": {
     "search": "filt"
   }
}
```

### Module filter
The Module filter only keeps identifiers that appear in the listed modules.

```json
{
   "filter": "modules",
   "params": {
     "modules": ["My.Module"]
   }
}
```

### Dependency filter
The Dependency filter only keeps identifiers that appear in the listed modules
and in any of their dependencies/imports.

```json
{
  "filter": "dependencies",
  "params": {
    "modules": ["My.Module"]
  }
}
```

### Namespace filter
The Namespace filter only keeps identifiers that appear in the listed namespaces.
Valid namespaces are `value`, `type` and `kind`.

```json
{
   "filter": "namespace",
   "params": {
     "namespaces": ["value", "type", "kind"]
   }
}
```

### Declaration type filter
A filter which allows to filter type declarations. Valid type declarations are
`value`, `type`, `synonym`, `dataconstructor`, `typeclass`, `valueoperator`,
`typeoperator` and `kind`.

```json
{
  "filter": "declarations",
  "params": [
    {
      "declarationtype": "value"
    },
    {
      "declarationtype": "type"
    },
    {
      "declarationtype": "synonym"
    },
    {
      "declarationtype": "dataconstructor"
    }
    {
      "declarationtype": "typeclass"
    },
    {
      "declarationtype": "valueoperator"
    },
    {
      "declarationtype": "typeoperator"
    },
    {
      "declarationtype": "kind"
    }
  ]
}
```

## Matcher:

### Flex matcher
Matches any occurence of the search string with intersections

The scoring measures how far the matches span the string, where
closer is better. The matches then get sorted with highest score first.

Examples:
- flMa matches **fl**ex**Ma**tcher. Score: 14.28
- sons matches **so**rtCompletio**ns**. Score: 6.25
```json

{
  "matcher": "flex",
  "params": {
    "search": "filt"
  }
}
```

### Distance Matcher

The Distance matcher is meant to provide corrections for typos. It calculates
the edit distance in between the search and the loaded identifiers.

```json
{
  "matcher": "distance",
  "params": {
    "search": "dilterM",
    "maximumDistance": 3
  }
}
```

## Responses

All Responses are wrapped in the following format:

```json
{
  "resultType": "success|error",
  "result": Result|Error
}
```

## CompletionOptions

Completion options allow to configure the number of returned completion results.

- maxResults :: Maybe Int

If specified limits the number of completion results, otherwise return all
results.

- groupReexports :: Maybe Boolean (defaults to False)

If set to True, groups all reexports of an identifier under the module it
originated from (the original export is also treated as a "reexport"). These
reexports then populate the `exportedFrom` field in their completion results and
the `module` field contains the originating module.

### Error

Errors at this point are merely Error strings. Newlines are escaped like `\n`
and should be taken care of by the editor-plugin.
