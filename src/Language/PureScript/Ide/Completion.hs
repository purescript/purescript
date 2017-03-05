{-# LANGUAGE PackageImports #-}
module Language.PureScript.Ide.Completion
       ( getCompletions
       , getExactMatches
       , completeInFile
       , insertHole
       ) where

import           Protolude

import           Data.Char
import           "monad-logger" Control.Monad.Logger
import qualified Data.Text as T
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Rebuild
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P

type Module = (P.ModuleName, [IdeDeclarationAnn])
-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions
  :: [Filter]
  -> Matcher IdeDeclarationAnn
  -> [Module]
  -> [Match IdeDeclarationAnn]
getCompletions filters matcher modules =
  runMatcher matcher (completionsFromModules (applyFilters filters modules))

getExactMatches :: Text -> [Filter] -> [Module] -> [Match IdeDeclarationAnn]
getExactMatches search filters modules =
  completionsFromModules (applyFilters (equalityFilter search : filters) modules)

completionsFromModules :: [Module] -> [Match IdeDeclarationAnn]
completionsFromModules = foldMap completionFromModule
  where
    completionFromModule (moduleName, decls) =
      map (\x -> Match (moduleName, x)) decls

-- | Attempts to complete a record accessor or record wildcard in the given file
-- at the given position. Uses the typed hole machinery in combination with the
-- Rebuild command, which makes this completion dependent on successful
-- compilation of the module once the typed hole has been inserted.
completeInFile
  :: (Ide m, MonadLogger m, MonadError IdeError m)
  => FilePath
  -> Int
  -> Int
  -> m [Completion]
completeInFile path row col = do
  input <- ideReadFile path
  completeInFile' path input row col

completeInFile'
  :: (Ide m, MonadLogger m)
  => FilePath -> Text -> Int -> Int -> m [Completion]
completeInFile' path input row column = do
  let withHole = insertHole (row, column) input
  rebuildResult <- runExceptT (rebuildFile' path (const (pure ())) withHole)
  case rebuildResult of
    Left (RebuildError errs)
      | Just holeError <- extractHole errs -> pure (extractCompletions holeError)
    _ -> pure []
  where

    extractHole :: P.MultipleErrors -> Maybe P.Type
    extractHole me = asum (map pscIdeHole (P.runMultipleErrors me))

    extractCompletions :: P.Type -> [Completion]
    extractCompletions =
      map mkCompletion
      . map (first P.prettyPrintLabel)
      . fst
      . P.rowToList
      where
        mkCompletion :: (Text, P.Type) -> Completion
        mkCompletion (i, t) = Completion
          { complModule = ""
          , complIdentifier = i
          , complType = prettyTypeT t
          , complExpandedType = prettyTypeT t
          , complLocation = Nothing
          , complDocumentation = Nothing
          }

insertHole :: (Int, Int) -> Text -> Text
insertHole (row, column) t =
  let
    (before, line:after) = splitAt (row - 1) (T.lines t)
    (b, a) = T.splitAt (column - 1) line
    (start, ident) = breakEnd (not . isSpace) b
    withHole = case ident of
      -- in this special case we are looking at syntax sugar for a record
      -- accessor function
      "_" ->
        start <> "?pscIdeHole" <> T.tail a
      i ->
        start <> "(?pscIdeHole " <> i <> ")" <> T.tail a
  in
    T.unlines (before <> [withHole] <> after)

breakEnd :: (Char -> Bool) -> Text -> (Text, Text)
breakEnd p t = (T.dropWhileEnd p t, T.takeWhileEnd p t)

pscIdeHole :: P.ErrorMessage -> Maybe P.Type
pscIdeHole (P.ErrorMessage _
                 (P.HoleInferredType "pscIdeHole"
                  (P.TypeApp (P.TypeApp t' (P.TypeApp r t)) _) _ _))
  | t' == P.tyFunction && r == P.tyRecord = Just t
pscIdeHole _ = Nothing
