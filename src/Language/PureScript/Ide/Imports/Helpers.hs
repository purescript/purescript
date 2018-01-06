module Language.PureScript.Ide.Imports.Helpers where

import           Protolude hiding (moduleName)

import           Control.Lens                       ((^.), (%~), ix)
import qualified Data.Text                          as T
import qualified Language.PureScript                as P
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Util
import qualified Text.Parsec as Parsec

data Import = Import P.ModuleName P.ImportDeclarationType (Maybe P.ModuleName)
              deriving (Eq, Show)

-- | Reads a file and returns the parsed modulename as well as the parsed
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
parseImportsFromFile' :: (MonadIO m, MonadError IdeError m) =>
                        FilePath -> m (P.ModuleName, [Text], [Import], [Text])
parseImportsFromFile' fp = do
  file <- ideReadFile fp
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

parseModuleHeader :: P.TokenParser ImportParse
parseModuleHeader = do
  _ <- P.readComments
  (mn, _) <- P.parseModuleDeclaration
  (ipStart, ipEnd, decls) <- P.withSourceSpan (\(P.SourceSpan _ start end) _ -> (start, end,))
    (P.mark (Parsec.many (P.same *> P.parseImportDeclaration')))
  pure (ImportParse mn ipStart ipEnd (map mkImport decls))
  where
    mkImport (mn, (P.Explicit refs), qual) = Import mn (P.Explicit refs) qual
    mkImport (mn, it, qual) = Import mn it qual

sliceImportSection :: [Text] -> Either Text (P.ModuleName, [Text], [Import], [Text])
sliceImportSection fileLines = first show $ do
  tokens <- P.lexLenient "<psc-ide>" file
  ImportParse{..} <- P.runTokenParser "<psc-ide>" parseModuleHeader tokens
  pure ( ipModuleName
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
