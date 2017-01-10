{-# LANGUAGE PackageImports #-}
module Language.PureScript.Ide.Completion
       ( getCompletions
       , getExactMatches
       , completeInFile
       ) where

import           Protolude

import Data.Char
import "monad-logger" Control.Monad.Logger
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

completeInFile :: (Ide m, MonadLogger m) => FilePath -> Int -> Int -> m [Completion]
completeInFile path row col = do
  input <- liftIO (readUTF8FileT path)
  completeInFile' path input row col

completeInFile' :: (Ide m, MonadLogger m) => FilePath -> Text -> Int -> Int -> m [Completion]
completeInFile' path input row col = do
  let withHole = insertHole input
  rebuildResult <- runExceptT (rebuildFile' path withHole)
  case rebuildResult of
    Left (RebuildError errs)
      | Just holeError <- extractHole errs
        -> do
          pure (extractCompletions holeError)
    _ -> pure []
  where
    insertHole :: Text -> Text
    insertHole t =
      let
        (before, line:after) = splitAt (row - 1) (T.lines t)
        (b, a) = T.splitAt (col - 1) line
        (start, ident) = breakEnd (not . isSpace) b
        withHole = start <> "(?magicUnicornHole " <> ident <> ")" <> T.tail a
      in
        T.unlines (before <> [withHole] <> after)

    extractHole :: P.MultipleErrors -> Maybe P.Type
    extractHole me = asum (map unicornTypeHole (P.runMultipleErrors me))

    extractCompletions :: P.Type -> [Completion]
    extractCompletions =
      map mkCompletion
      . map (first (P.prettyPrintStringJS . P.runLabel))
      . fst
      . P.rowToList
      where
        mkCompletion :: (Text, P.Type) -> Completion
        mkCompletion (i, t) = Completion
          { complModule = ""
          , complIdentifier = T.filter (/= '"') i
          , complType = prettyTypeT t
          , complExpandedType = prettyTypeT t
          , complLocation = Nothing
          , complDocumentation = Nothing
          }

breakEnd :: (Char -> Bool) -> Text -> (Text, Text)
breakEnd p t = (T.dropWhileEnd p t, T.takeWhileEnd p t)

unicornTypeHole :: P.ErrorMessage -> Maybe P.Type
unicornTypeHole (P.ErrorMessage _
                 (P.HoleInferredType "magicUnicornHole"
                  (P.TypeApp (P.TypeApp t' (P.TypeApp r t)) _) _ _))
  | t' == P.tyFunction && r == P.tyRecord = Just t
unicornTypeHole _ = Nothing
