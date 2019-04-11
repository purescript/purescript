module Language.PureScript.CST
  ( parseModuleFromFile
  , parseModulesFromFiles
  , unwrapParserError
  , module Language.PureScript.CST.Convert
  , module Language.PureScript.CST.Errors
  , module Language.PureScript.CST.Parser
  , module Language.PureScript.CST.Types
  ) where

import Prelude

import Control.Monad.Error.Class (MonadError(..))
import Control.Parallel.Strategies (withStrategy, parList, evalTuple2, r0, rseq)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Language.PureScript.AST as AST
import qualified Language.PureScript.Errors as E
import Language.PureScript.CST.Convert
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Parser
import Language.PureScript.CST.Types

parseModulesFromFiles
  :: forall m k
   . MonadError E.MultipleErrors m
  => (k -> FilePath)
  -> [(k, Text)]
  -> m [(k, PartialResult AST.Module)]
parseModulesFromFiles toFilePath input =
  flip E.parU handleError . inParallel . flip fmap input $ parseModuleFromFile toFilePath
  where
  handleError :: (k, Either (NE.NonEmpty ParserError) a) -> m (k, a)
  handleError (k, res) = (k,) <$> unwrapParserError (toFilePath k) res

  inParallel :: [(k, Either (NE.NonEmpty ParserError) a)] -> [(k, Either (NE.NonEmpty ParserError) a)]
  inParallel = withStrategy (parList (evalTuple2 r0 rseq))

parseModuleFromFile
  :: forall k
   . (k -> FilePath)
  -> (k, Text)
  -> (k, Either (NE.NonEmpty ParserError) (PartialResult AST.Module))
parseModuleFromFile toFilePath (k, content) =
  (k, fmap (convertModule (toFilePath k)) <$> parseModule content)

unwrapParserError
  :: forall m a
   . MonadError E.MultipleErrors m
  => FilePath
  -> Either (NE.NonEmpty ParserError) a
  -> m a
unwrapParserError fp =
  either (throwError . E.MultipleErrors . NE.toList . fmap (toPositionedError fp)) pure

toPositionedError :: FilePath -> ParserError -> E.ErrorMessage
toPositionedError name perr =
  E.ErrorMessage [E.positionedError $ sourceSpan name $ errRange perr] (E.ErrorParsingCSTModule perr)
