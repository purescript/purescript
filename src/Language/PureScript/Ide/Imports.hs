-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Imports
-- Description : Provides functionality to manage imports
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Provides functionality to manage imports
-----------------------------------------------------------------------------

module Language.PureScript.Ide.Imports
       ( parseImportsFromFile
       , parseImportsFromFile'
         -- for tests
       , parseImport
       , prettyPrintImportSection
       , sliceImportSection
       , prettyPrintImport'
       , Import(Import)
       )
       where

import Protolude hiding (moduleName)

import Control.Lens ((^.), (%~), ix)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Ide.Error (IdeError(..))
import Language.PureScript.Ide.Util (ideReadFile)

data Import = Import P.ModuleName P.ImportDeclarationType (Maybe P.ModuleName)
              deriving (Eq, Show)

-- | Reads a file and returns the parsed module name as well as the parsed
-- imports, while ignoring eventual parse errors that aren't relevant to the
-- import section
parseImportsFromFile
  :: (MonadIO m, MonadError IdeError m)
  => FilePath
  -> m (P.ModuleName, [(P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName)])
parseImportsFromFile file = do
  (mn, _, imports, _) <- parseImportsFromFile' file
  pure (mn, unwrapImport <$> imports)
  where
    unwrapImport (Import a b c) = (a, b, c)

-- | Reads a file and returns the (lines before the imports, the imports, the
-- lines after the imports)
parseImportsFromFile'
  :: (MonadIO m, MonadError IdeError m)
  => FilePath
  -> m (P.ModuleName, [Text], [Import], [Text])
parseImportsFromFile' fp = do
  (_, file) <- ideReadFile fp
  case sliceImportSection (T.lines file) of
    Right res -> pure res
    Left err -> throwError (GeneralError err)

-- | @ImportParse@ holds the data we extract out of a partial parse of the
-- sourcefile
data ImportParse = ImportParse
  { ipModuleName :: P.ModuleName
  -- ^ the module name we parse
  , ipStart :: P.SourcePos
  -- ^ the beginning of the import section. If `import Prelude` was the first
  -- import, this would point at `i`
  , ipEnd :: P.SourcePos
  -- ^ the end of the import section
  , ipImports :: [Import]
  -- ^ the extracted import declarations
  }

parseModuleHeader :: Text -> Either (NE.NonEmpty CST.ParserError) ImportParse
parseModuleHeader src = do
  CST.PartialResult md _ <- CST.parseModule $ CST.lenient $ CST.lexModule src
  let
    mn = CST.nameValue $ CST.modNamespace md
    decls = flip fmap (CST.modImports md) $ \decl -> do
      let ((ss, _), mn', it, qual) = CST.convertImportDecl "<purs-ide>" decl
      (ss, Import mn' it qual)
  case (head decls, lastMay decls) of
    (Just hd, Just ls) -> do
      let
        ipStart = P.spanStart $ fst hd
        ipEnd = P.spanEnd $ fst ls
      pure $ ImportParse mn ipStart ipEnd $ snd <$> decls
    _ -> do
      let pos = CST.sourcePos . CST.srcEnd . CST.tokRange . CST.tokAnn $ CST.modWhere md
      pure $ ImportParse mn pos pos []

sliceImportSection :: [Text] -> Either Text (P.ModuleName, [Text], [Import], [Text])
sliceImportSection fileLines = first (toS . CST.prettyPrintError . NE.head) $ do
  ImportParse{..} <- parseModuleHeader file
  pure
    ( ipModuleName
    , sliceFile (P.SourcePos 1 1) (prevPos ipStart)
    , ipImports
    -- Not sure why I need to drop 1 here, but it makes the tests pass
    , drop 1 (sliceFile (nextPos ipEnd) (P.SourcePos (length fileLines) (lineLength (length fileLines))))
    )
  where
    prevPos (P.SourcePos l c)
      | l == 1 && c == 1 = P.SourcePos l c
      | c == 1 = P.SourcePos (l - 1) (lineLength (l - 1))
      | otherwise = P.SourcePos l (c - 1)
    nextPos (P.SourcePos l c)
      | c == lineLength l = P.SourcePos (l + 1) 1
      | otherwise = P.SourcePos l (c + 1)
    file = T.unlines fileLines
    lineLength l = T.length (fileLines ^. ix (l - 1))
    sliceFile (P.SourcePos l1 c1) (P.SourcePos l2 c2) =
      fileLines
      & drop (l1 - 1)
      & take (l2 - l1 + 1)
      & ix 0 %~ T.drop (c1 - 1)
      & ix (l2 - l1) %~ T.take c2

prettyPrintImport' :: Import -> Text
prettyPrintImport' (Import mn idt qual) =
  "import " <> P.prettyPrintImport mn idt qual

prettyPrintImportSection :: [Import] -> [Text]
prettyPrintImportSection imports =
  let
    (implicitImports, explicitImports) = partition isImplicitImport imports
  in
    sort (map prettyPrintImport' implicitImports)
      -- Only add the extra spacing if both implicit as well as
      -- explicit/qualified imports exist
      <> (guard (not (null explicitImports || null implicitImports)) $> "")
      <> sort (map prettyPrintImport' explicitImports)
  where
    isImplicitImport :: Import -> Bool
    isImplicitImport i = case i of
      Import _ P.Implicit Nothing -> True
      Import _ (P.Hiding _) Nothing -> True
      _ -> False

-- | Test and ghci helper
parseImport :: Text -> Maybe Import
parseImport t =
  case fmap (CST.convertImportDecl "<purs-ide>" . snd)
        $ CST.runTokenParser CST.parseImportDeclP
        $ CST.lex t of
    Right (_, mn, idt, mmn) ->
      Just (Import mn idt mmn)
    _ -> Nothing
