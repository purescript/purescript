-- |
-- Common code generation utility functions
--
module Language.PureScript.CodeGen.JS.Common where

import Prelude.Compat

import Data.Char
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Names

moduleNameToJs :: ModuleName -> Text
moduleNameToJs (ModuleName pns) =
  let name = T.intercalate "_" (runProperName `map` pns)
  in if nameIsJsBuiltIn name then "$$" <> name else name

-- |
-- Convert an Ident into a valid JavaScript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToJs :: Ident -> Text
identToJs (Ident name) = properToJs name
identToJs (GenIdent _ _) = internalError "GenIdent in identToJs"

properToJs :: Text -> Text
properToJs name
  | nameIsJsReserved name || nameIsJsBuiltIn name = "$$" <> name
  | otherwise = T.concatMap identCharToText name

-- |
-- Test if a string is a valid JS identifier without escaping.
--
identNeedsEscaping :: Text -> Bool
identNeedsEscaping s = s /= properToJs s || T.null s

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToText :: Char -> Text
identCharToText c | isAlphaNum c = T.singleton c
identCharToText '_' = "_"
identCharToText '.' = "$dot"
identCharToText '$' = "$dollar"
identCharToText '~' = "$tilde"
identCharToText '=' = "$eq"
identCharToText '<' = "$less"
identCharToText '>' = "$greater"
identCharToText '!' = "$bang"
identCharToText '#' = "$hash"
identCharToText '%' = "$percent"
identCharToText '^' = "$up"
identCharToText '&' = "$amp"
identCharToText '|' = "$bar"
identCharToText '*' = "$times"
identCharToText '/' = "$div"
identCharToText '+' = "$plus"
identCharToText '-' = "$minus"
identCharToText ':' = "$colon"
identCharToText '\\' = "$bslash"
identCharToText '?' = "$qmark"
identCharToText '@' = "$at"
identCharToText '\'' = "$prime"
identCharToText c = '$' `T.cons` T.pack (show (ord c))

-- |
-- Checks whether an identifier name is reserved in JavaScript.
--
nameIsJsReserved :: Text -> Bool
nameIsJsReserved name =
  name `elem` jsAnyReserved

-- |
-- Checks whether a name matches a built-in value in JavaScript.
--
nameIsJsBuiltIn :: Text -> Bool
nameIsJsBuiltIn name =
  name `elem`
    [ "arguments"
    , "Array"
    , "ArrayBuffer"
    , "Boolean"
    , "DataView"
    , "Date"
    , "decodeURI"
    , "decodeURIComponent"
    , "encodeURI"
    , "encodeURIComponent"
    , "Error"
    , "escape"
    , "eval"
    , "EvalError"
    , "Float32Array"
    , "Float64Array"
    , "Function"
    , "Infinity"
    , "Int16Array"
    , "Int32Array"
    , "Int8Array"
    , "Intl"
    , "isFinite"
    , "isNaN"
    , "JSON"
    , "Map"
    , "Math"
    , "NaN"
    , "Number"
    , "Object"
    , "parseFloat"
    , "parseInt"
    , "Promise"
    , "Proxy"
    , "RangeError"
    , "ReferenceError"
    , "Reflect"
    , "RegExp"
    , "Set"
    , "SIMD"
    , "String"
    , "Symbol"
    , "SyntaxError"
    , "TypeError"
    , "Uint16Array"
    , "Uint32Array"
    , "Uint8Array"
    , "Uint8ClampedArray"
    , "undefined"
    , "unescape"
    , "URIError"
    , "WeakMap"
    , "WeakSet"
    ]

jsAnyReserved :: [Text]
jsAnyReserved =
  concat
    [ jsKeywords
    , jsSometimesReserved
    , jsFutureReserved
    , jsFutureReservedStrict
    , jsOldReserved
    , jsLiterals
    ]

jsKeywords :: [Text]
jsKeywords =
  [ "break"
  , "case"
  , "catch"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "export"
  , "extends"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "instanceof"
  , "new"
  , "return"
  , "super"
  , "switch"
  , "this"
  , "throw"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"
  ]

jsSometimesReserved :: [Text]
jsSometimesReserved =
  [ "await"
  , "let"
  , "static"
  , "yield"
  ]

jsFutureReserved :: [Text]
jsFutureReserved =
  [ "enum" ]

jsFutureReservedStrict :: [Text]
jsFutureReservedStrict =
  [ "implements"
  , "interface"
  , "package"
  , "private"
  , "protected"
  , "public"
  ]

jsOldReserved :: [Text]
jsOldReserved =
  [ "abstract"
  , "boolean"
  , "byte"
  , "char"
  , "double"
  , "final"
  , "float"
  , "goto"
  , "int"
  , "long"
  , "native"
  , "short"
  , "synchronized"
  , "throws"
  , "transient"
  , "volatile"
  ]

jsLiterals :: [Text]
jsLiterals =
  [ "null"
  , "true"
  , "false"
  ]
