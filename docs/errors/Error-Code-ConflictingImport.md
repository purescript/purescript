This error can occur when you attempt to define a value with the same name as one which has already been imported.

For example, `Prelude` exports a function called `id`. If I attempt to define my own `id` as well, like this:

```purescript
module ConflictingImport where

id :: Number
id = 1
```

Then this will fail with:

```
>> Error in module ConflictingImport:
>> Declaration id conflicts with import ConflictingImport
```

One potential solution is to hide the problematic imports with a `hiding` list:

```purescript
module NoConflictingImport where

import Prelude hiding (id)

id :: Number
id = 1
```

Alternatively, we can import one of the two modules which define `id` as "qualified":

```purescript
module NoConflictingImport where

import qualified Prelude as P

id :: Number
id = 1
```

In the context of PSCI, a function named "main" is pre-defined by PSCI. Therefore, importing a module which also defines a function named "main" will cause PSCI to emit this ConflictingImport error. This error can be resolved by importing the external module using the `qualified` or `hiding` keywords, as shown above.