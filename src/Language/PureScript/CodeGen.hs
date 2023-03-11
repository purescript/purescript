-- |
-- A collection of modules related to code generation:
--
--  [@Language.PureScript.CodeGen.JS@] Code generator for JavaScript
--
module Language.PureScript.CodeGen (module C) where

import Language.PureScript.CodeGen.JS as C
    ( anyNameToJs,
      identCharToText,
      identToJs,
      isValidJsIdentifier,
      jsAnyReserved,
      jsFutureReserved,
      jsFutureReservedStrict,
      jsKeywords,
      jsLiterals,
      jsOldReserved,
      jsSometimesReserved,
      moduleNameToJs,
      nameIsJsBuiltIn,
      nameIsJsReserved,
      properToJs,
      everywhere,
      everywhereTopDownM,
      withSourceSpan,
      AST,
      InitializerEffects(..),
      moduleToJs )
