-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Errors.JSON
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Errors.JSON where

import Prelude ()
import Prelude.Compat

import qualified Data.Aeson.TH as A

import qualified Language.PureScript as P

data ErrorPosition = ErrorPosition
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  } deriving (Show, Eq, Ord)

data ErrorSuggestion = ErrorSuggestion { replacement :: String } deriving (Show, Eq)

data JSONError = JSONError
  { position :: Maybe ErrorPosition
  , message :: String
  , errorCode :: String
  , errorLink :: String
  , filename :: Maybe String
  , moduleName :: Maybe String
  , suggestion :: Maybe ErrorSuggestion
  }  deriving (Show, Eq)

data JSONResult = JSONResult
  { warnings :: [JSONError]
  , errors :: [JSONError]
  } deriving (Show, Eq)

$(A.deriveJSON A.defaultOptions ''ErrorPosition)
$(A.deriveJSON A.defaultOptions ''JSONError)
$(A.deriveJSON A.defaultOptions ''JSONResult)
$(A.deriveJSON A.defaultOptions ''ErrorSuggestion)


toJSONErrors :: Bool -> P.Level -> P.MultipleErrors -> [JSONError]
toJSONErrors verbose level = map (toJSONError verbose level) . P.runMultipleErrors

toJSONError :: Bool -> P.Level -> P.ErrorMessage -> JSONError
toJSONError verbose level e =
  JSONError (toErrorPosition <$> sspan)
            (P.renderBox (P.prettyPrintSingleError verbose level False (P.stripModuleAndSpan e)))
            (P.errorCode e)
            (P.wikiUri e)
            (P.spanName <$> sspan)
            (P.runModuleName <$> P.errorModule e)
            (toSuggestion <$> P.errorSuggestion (P.unwrapErrorMessage e))
  where
  sspan :: Maybe P.SourceSpan
  sspan = P.errorSpan e

  toErrorPosition :: P.SourceSpan -> ErrorPosition
  toErrorPosition ss =
    ErrorPosition (P.sourcePosLine   (P.spanStart ss))
                  (P.sourcePosColumn (P.spanStart ss))
                  (P.sourcePosLine   (P.spanEnd   ss))
                  (P.sourcePosColumn (P.spanEnd   ss))
  toSuggestion :: P.ErrorSuggestion -> ErrorSuggestion
-- TODO: Adding a newline because source spans chomp everything up to the next character
  toSuggestion (P.ErrorSuggestion s) = ErrorSuggestion $ if null s then s else s ++ "\n"
