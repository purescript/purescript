{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Errors.JSON where

import Prelude.Compat

import qualified Data.Aeson.TH as A
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)

import qualified Language.PureScript as P

data ErrorPosition = ErrorPosition
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  } deriving (Show, Eq, Ord)

data ErrorSuggestion = ErrorSuggestion
  { replacement :: Text
  , replaceRange :: Maybe ErrorPosition
  } deriving (Show, Eq)

data JSONError = JSONError
  { position :: Maybe ErrorPosition
  , message :: String
  , errorCode :: Text
  , errorLink :: Text
  , filename :: Maybe String
  , moduleName :: Maybe Text
  , suggestion :: Maybe ErrorSuggestion
  , allSpans :: [P.SourceSpan]
  } deriving (Show, Eq)

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
  JSONError (toErrorPosition <$> fmap NEL.head spans)
            (P.renderBox (P.prettyPrintSingleError (P.PPEOptions Nothing verbose level False mempty) (P.stripModuleAndSpan e)))
            (P.errorCode e)
            (P.errorDocUri e)
            (P.spanName <$> fmap NEL.head spans)
            (P.runModuleName <$> P.errorModule e)
            (toSuggestion e)
            (maybe [] NEL.toList spans)
  where
  spans :: Maybe (NEL.NonEmpty P.SourceSpan)
  spans = P.errorSpan e

  toErrorPosition :: P.SourceSpan -> ErrorPosition
  toErrorPosition ss =
    ErrorPosition (P.sourcePosLine   (P.spanStart ss))
                  (P.sourcePosColumn (P.spanStart ss))
                  (P.sourcePosLine   (P.spanEnd   ss))
                  (P.sourcePosColumn (P.spanEnd   ss))
  toSuggestion :: P.ErrorMessage -> Maybe ErrorSuggestion
  toSuggestion em =
    case P.errorSuggestion $ P.unwrapErrorMessage em of
      Nothing -> Nothing
      Just s -> Just $ ErrorSuggestion (suggestionText s) (toErrorPosition <$> P.suggestionSpan em)

  suggestionText (P.ErrorSuggestion s) = s
