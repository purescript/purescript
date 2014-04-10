-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common code generation utility functions
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Common where

import Data.Char
import Data.List (intercalate)
import Language.PureScript.Names

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
identToJs (Ident name) | nameIsJsReserved name = "$$" ++ name
identToJs (Ident name) = concatMap identCharToString name
identToJs (Op op) = concatMap identCharToString op

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
  name `elem` [ "abstract"
              , "arguments"
              , "boolean"
              , "break"
              , "byte"
              , "case"
              , "catch"
              , "char"
              , "class"
              , "const"
              , "continue"
              , "debugger"
              , "default"
              , "delete"
              , "do"
              , "double"
              , "else"
              , "enum"
              , "eval"
              , "export"
              , "extends"
              , "final"
              , "finally"
              , "float"
              , "for"
              , "function"
              , "goto"
              , "if"
              , "implements"
              , "import"
              , "in"
              , "instanceof"
              , "int"
              , "interface"
              , "let"
              , "long"
              , "native"
              , "new"
              , "null"
              , "package"
              , "private"
              , "protected"
              , "public"
              , "return"
              , "short"
              , "static"
              , "super"
              , "switch"
              , "synchronized"
              , "this"
              , "throw"
              , "throws"
              , "transient"
              , "try"
              , "typeof"
              , "var"
              , "void"
              , "volatile"
              , "while"
              , "with"
              , "yield" ]

-- |
-- Test if a string is a valid JS identifier (may return false negatives)
--
isIdent :: String -> Bool
isIdent s@(first : rest) | not (nameIsJsReserved s) && isAlpha first && all isAlphaNum rest = True
isIdent _ = False

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) = intercalate "_" (runProperName `map` pns)
