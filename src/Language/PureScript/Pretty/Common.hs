-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common pretty-printing utility functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

module Language.PureScript.Pretty.Common where

import Data.Char
import Control.Monad.State
import qualified Control.Category as C
import Control.Category ((>>>))
import qualified Control.Arrow as A
import Control.Arrow ((***), (<+>))

import Language.PureScript.Names

-- |
-- Convert an Ident into a valid Javascript identifier:
--
--  * Alphanumeric characters are kept unmodified
--
--  * Symbols are encoded as a dollar symbol ($) followed by their ordinal value
--
identToJs :: Ident -> String
identToJs (Ident name) = concatMap identCharToString name
identToJs (Op op) = concatMap identCharToString op

identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_' = "_"
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
identCharToString c = '$' : show (ord c)

-- |
-- Wrap a string in parentheses
--
parens :: String -> String
parens s = ('(':s) ++ ")"
