Add the export to the list of exported functions.

E.g. if getting:

    Error in module ReCQ:
    An export for execCommand requires the following to also be exported:
    JSCmd

For

``` {.haskell}
module ReCQ export (execCommand) where
import Prelude
data JSCmd = JSCmd { type :: String }
execCommand :: JSCmd -> Unit
execCommand _ -> unit
```

add `JSCmd(..)` to the list: `module ReCQ (execCommand, JSCmd(..)) ...`.
You might be tempted to write `JSCmd` and be done with it, but beware of
the dragon lurking: it will be interpreted as the *typeclass* JSCmd,
which this is not.

(See the lang guide on
Modules)[https://github.com/purescript/purescript/wiki/Language-Guide:-Modules\#importing-modules]

