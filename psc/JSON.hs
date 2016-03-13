-----------------------------------------------------------------------------
--
-- Module      :  Main
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module JSON where

import Prelude ()
import Prelude.Compat

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A

import qualified Language.PureScript as P

data ErrorPosition = ErrorPosition
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

data ErrorSuggestion = ErrorSuggestion { replacement :: String }

data JSONError = JSONError
  { position :: Maybe ErrorPosition
  , message :: String
  , errorCode :: String
  , errorLink :: String
  , filename :: Maybe String
  , moduleName :: Maybe String
  , suggestion :: Maybe ErrorSuggestion
  }

-- These accessors are par
data JSONWarnings
  = JSONWarningSummary Int
  | JSONWarnings [JSONError]

instance A.ToJSON JSONWarnings where
  toJSON (JSONWarningSummary n) = A.object ["count" .= n]
  toJSON (JSONWarnings w) = A.toJSON w

data JSONResult = JSONResult
  { warnings :: JSONWarnings
  , errors :: [JSONError]
  }

$(A.deriveToJSON A.defaultOptions ''ErrorPosition)
$(A.deriveToJSON A.defaultOptions ''JSONError)
$(A.deriveToJSON A.defaultOptions ''JSONResult)
$(A.deriveToJSON A.defaultOptions ''ErrorSuggestion)

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
            (toSuggestion <$> (P.errorSuggestion $ P.unwrapErrorMessage e))
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
