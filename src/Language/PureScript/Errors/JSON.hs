{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Errors.JSON where

import Prelude

import Data.Aeson.TH qualified as A
import Data.List.NonEmpty qualified as NEL
import Data.Text (Text)

import Language.PureScript.AST.SourcePos ( SourcePos(sourcePosColumn, sourcePosLine), SourceSpan(spanName, spanStart, spanEnd) )
import Language.PureScript.Errors qualified as PErr
import Language.PureScript.Names qualified as PN

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
  , allSpans :: [SourceSpan]
  } deriving (Show, Eq)

data JSONResult = JSONResult
  { warnings :: [JSONError]
  , errors :: [JSONError]
  } deriving (Show, Eq)

$(A.deriveJSON A.defaultOptions ''ErrorPosition)
$(A.deriveJSON A.defaultOptions ''ErrorSuggestion)
$(A.deriveJSON A.defaultOptions ''JSONError)
$(A.deriveJSON A.defaultOptions ''JSONResult)

toJSONErrors :: Bool -> PErr.Level -> [(FilePath, Text)] -> PErr.MultipleErrors -> [JSONError]
toJSONErrors verbose level files = map (toJSONError verbose level files) . PErr.runMultipleErrors

toJSONError :: Bool -> PErr.Level -> [(FilePath, Text)] -> PErr.ErrorMessage -> JSONError
toJSONError verbose level files e =
  JSONError (toErrorPosition <$> fmap NEL.head spans)
            (PErr.renderBox (PErr.prettyPrintSingleError (PErr.PPEOptions Nothing verbose level False mempty files) (PErr.stripModuleAndSpan e)))
            (PErr.errorCode e)
            (PErr.errorDocUri e)
            (spanName <$> fmap NEL.head spans)
            (PN.runModuleName <$> PErr.errorModule e)
            (toSuggestion e)
            (maybe [] NEL.toList spans)
  where
  spans :: Maybe (NEL.NonEmpty SourceSpan)
  spans = PErr.errorSpan e

  toErrorPosition :: SourceSpan -> ErrorPosition
  toErrorPosition ss =
    ErrorPosition (sourcePosLine   (spanStart ss))
                  (sourcePosColumn (spanStart ss))
                  (sourcePosLine   (spanEnd   ss))
                  (sourcePosColumn (spanEnd   ss))
  toSuggestion :: PErr.ErrorMessage -> Maybe ErrorSuggestion
  toSuggestion em =
    case PErr.errorSuggestion $ PErr.unwrapErrorMessage em of
      Nothing -> Nothing
      Just s -> Just $ ErrorSuggestion (suggestionText s) (toErrorPosition <$> PErr.suggestionSpan em)

  suggestionText (PErr.ErrorSuggestion s) = s
