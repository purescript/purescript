{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}

module Language.PureScript.Ide.Imports
       ( addImplicitImport
       )
       where

import Language.PureScript
import Language.PureScript.Ide.Error
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text as T

data Import = Import ModuleName ImportDeclarationType (Maybe ModuleName)
              deriving (Eq, Show)


-- | Parses a file and returns the (lines before the imports, the imports, the
-- lines after the imports)
parseImportsFromFile :: (MonadIO m, MonadError PscIdeError m) =>
                        FilePath -> m ([Text], [Import], [Text])
parseImportsFromFile fp = do
  file <- liftIO (TIO.readFile fp)
  let fLines = T.lines file
      preImportSection = takeWhile (not . hasImportPrefix) fLines
      importSection =
        takeWhile continuesImport $
          dropWhile (not . hasImportPrefix) fLines
      postImportSection =
        dropWhile continuesImport $
          dropWhile (not . hasImportPrefix) fLines
      hasImportPrefix = T.isPrefixOf "import"
      continuesImport x = hasImportPrefix x || T.isPrefixOf " " x || x == ""

  pure (preImportSection, parseImports importSection, postImportSection)

parseImports :: [Text] -> [Import]
parseImports = mapMaybe parseImport

parseImport :: Text -> Maybe Import
parseImport t =
  let
    parseResult = do
      tokens <- Language.PureScript.lex "" (T.unpack t)
      runTokenParser "" parseImportDeclaration' tokens
  in
    case parseResult of
      Right (mn, idt, mmn, _) -> Just (Import mn idt mmn)
      Left _ -> Nothing

addImplicitImport :: (MonadIO m, MonadError PscIdeError m) =>
                     FilePath -> ModuleName -> m [Text]
addImplicitImport fp mn = do
  (pre, imports, post) <- parseImportsFromFile fp
  pure $ pre
    ++ sort (map prettyPrintImport' (imports ++ [Import mn Implicit Nothing]))
    ++ [""]
    ++ post

prettyPrintImport' :: Import -> Text
prettyPrintImport' (Import mn idt qual) = T.pack $ "import " ++ prettyPrintImport mn idt qual
