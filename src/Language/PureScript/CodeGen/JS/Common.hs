-- |
-- Common code generation utility functions
--
module Language.PureScript.CodeGen.JS.Common where

import Prelude.Compat

import Data.Char
import Data.List (intercalate)

import Language.PureScript.Crash
import Language.PureScript.Names

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) =
  let name = intercalate "_" (runProperName `map` pns)
  in if nameIsJsBuiltIn name then "$$" ++ name else name

-- |
-- Convert an Ident into a valid Javascript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToJs :: Ident -> String
identToJs (Ident name)
  | nameIsJsReserved name || nameIsJsBuiltIn name = "$$" ++ name
  | otherwise = concatMap identCharToString name
identToJs (GenIdent _ _) = internalError "GenIdent in identToJs"

-- |
-- Test if a string is a valid JS identifier without escaping.
--
identNeedsEscaping :: String -> Bool
identNeedsEscaping s = s /= identToJs (Ident s) || null s

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_' = "_"
identCharToString '.' = "$dot"
identCharToString '$' = "$dollar"
identCharToString '~' = "$tilde"
identCharToString '=' = "$eq"
identCharToString '<' = "$less"
identCharToString '>' = "$greater"
identCharToString '!' = "$bang"
identCharToString '#' = "$hash"
identCharToString '%' = "$percent"
identCharToString '^' = "$up"
identCharToString '&' = "$amp"
identCharToString '|' = "$bar"
identCharToString '*' = "$times"
identCharToString '/' = "$div"
identCharToString '+' = "$plus"
identCharToString '-' = "$minus"
identCharToString ':' = "$colon"
identCharToString '\\' = "$bslash"
identCharToString '?' = "$qmark"
identCharToString '@' = "$at"
identCharToString '\'' = "$prime"
identCharToString c = '$' : show (ord c)

-- |
-- Checks whether an identifier name is reserved in Javascript.
--
nameIsJsReserved :: String -> Bool
nameIsJsReserved name =
  name `elem` jsAnyReserved

-- |
-- Checks whether a name matches a built-in value in Javascript.
--
nameIsJsBuiltIn :: String -> Bool
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

jsAnyReserved :: [String]
jsAnyReserved =
  concat
    [ jsKeywords
    , jsSometimesReserved
    , jsFutureReserved
    , jsFutureReservedStrict
    , jsOldReserved
    , jsLiterals
    ]

jsKeywords :: [String]
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

jsSometimesReserved :: [String]
jsSometimesReserved =
  [ "await"
  , "let"
  , "static"
  , "yield"
  ]

jsFutureReserved :: [String]
jsFutureReserved =
  [ "enum" ]

jsFutureReservedStrict :: [String]
jsFutureReservedStrict =
  [ "implements"
  , "interface"
  , "package"
  , "private"
  , "protected"
  , "public"
  ]

jsOldReserved :: [String]
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

jsLiterals :: [String]
jsLiterals =
  [ "null"
  , "true"
  , "false"
  ]
