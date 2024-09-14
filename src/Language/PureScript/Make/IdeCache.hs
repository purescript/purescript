module Language.PureScript.Make.IdeCache where

import Prelude
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple qualified as SQLite
import Codec.Serialise qualified as Serialise
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import System.FilePath ((</>), takeDirectory)
import Language.PureScript.Names (runModuleName, ProperName (runProperName), runIdent)
import Language.PureScript.Externs (ExternsFile(..), ExternsImport(..))
import Data.Foldable (for_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Directory (createDirectoryIfMissing)
import Language.PureScript.Externs qualified as P
import Data.Text qualified as Text
import Data.Maybe (isNothing, fromMaybe)
import Language.PureScript.CST.Utils (ProperName(..))
import Language.PureScript.Docs.Types qualified as Docs
import Language.PureScript.Ide.Externs (convertExterns)
import Language.PureScript.Ide.Util (identifierFromIdeDeclaration, discardAnn)
import Data.Function ((&))
import Data.Bifunctor (first)
import Data.Text (Text)
import Language.PureScript.Ide.Types (idaDeclaration)
import Language.PureScript.Docs.Types (Declaration(declChildren))
import Language.PureScript.Docs.Render (renderDeclaration)
import Language.PureScript.Docs.AsMarkdown (codeToString, declAsMarkdown, runDocs)
import Codec.Serialise (serialise)

sqliteExtern :: (MonadIO m) => FilePath -> Docs.Module -> ExternsFile -> m ()
sqliteExtern outputDir docs extern = liftIO $ do 
    conn <- SQLite.open db
    withRetry $ SQLite.executeNamed conn
      "INSERT INTO modules (module_name, comment, extern, dec) VALUES (:module_name, :docs, :extern, :dec)" 
      [ ":module_name" :=  runModuleName ( efModuleName extern )
      , ":docs" := Docs.modComments docs
      , ":extern" := Serialise.serialise extern
      , ":dec" := show ( efExports extern )
      ]
    for_ (efImports extern) (\i -> do
       withRetry $ SQLite.executeNamed conn "INSERT INTO dependencies (module_name, dependency) VALUES (:module_name, :dependency)"
        [ ":module_name" := runModuleName (efModuleName extern )
        , ":dependency" := runModuleName (eiModule i)
        ])

    for_ (fst $ convertExterns extern) (\i -> do
       withRetry $ SQLite.executeNamed conn "INSERT INTO decla (module_name, id) VALUES (:module_name, :id)"
        [ ":module_name" := runModuleName (efModuleName extern )
        , ":id" := identifierFromIdeDeclaration (discardAnn i)
        ])

    for_ (Docs.modDeclarations docs) (\d -> do
       withRetry $ SQLite.executeNamed conn "INSERT INTO declarations (module_name, name, span, type, docs, declaration) VALUES (:module_name, :name, :span, :type, :docs, :declaration)"
        [ ":module_name" := runModuleName (efModuleName extern)
        , ":name" := Docs.declTitle d
        , ":span" := serialise (Docs.declSourceSpan d)
        , ":docs" := Docs.declComments d
        , ":type" := runDocs (declAsMarkdown d)
        , ":declaration" := show d
        ]
       

       for_ (declChildren d) $ \ch -> do
         withRetry $ SQLite.executeNamed conn "INSERT INTO declarations (module_name, name, span, docs, declaration) VALUES (:module_name, :name, :span, :docs, :declaration)"
          [ ":module_name" := runModuleName (efModuleName extern)
          , ":name" := Docs.cdeclTitle ch
          , ":span" := serialise (Docs.declSourceSpan d)
          , ":docs" := Docs.cdeclComments ch
          , ":declaration" := show d
          ]
        )

    SQLite.close conn
    return ()
  where
  db = outputDir </> "cache.db"


convertDecl :: P.ExternsDeclaration -> Text.Text
convertDecl = \case 
  P.EDType{..}  -> runProperName edTypeName
  P.EDDataConstructor{..}  -> runProperName edDataCtorName
  P.EDValue{..}  -> runIdent edValueName
  _ -> "OTHER"

spanDecl :: P.ExternsDeclaration -> Text.Text
spanDecl = \case
  _ -> "NO SPAN"

withRetry :: IO () -> IO ()
withRetry op = do
  r <- try op
  case r of
   Left (SQLite.SQLError SQLite.ErrorBusy _ _)  -> do 
     threadDelay 50
     withRetry op
     return ()
   Left _ -> do
     return ()
   Right qr -> return qr


createParentDirectory :: FilePath -> IO ()
createParentDirectory = createDirectoryIfMissing True . takeDirectory

sqliteInit :: (MonadIO m) => FilePath -> m ()
sqliteInit outputDir = liftIO $ do
    createParentDirectory db
    conn <- SQLite.open db
    withRetry $ SQLite.execute_ conn "pragma journal_mode=wal"
    withRetry $ SQLite.execute_ conn "create table if not exists modules (module_name text primary key, comment text, extern blob, dec text, unique (module_name) on conflict replace)"
    withRetry $ SQLite.execute_ conn "create table if not exists dependencies (id integer primary key, module_name text not null, dependency text not null, unique (module_name, dependency) on conflict ignore)"
    withRetry $ SQLite.execute_ conn "create table if not exists declarations (module_name text, name text not null, span blob, type text, docs text, declaration text not null)"
    withRetry $ SQLite.execute_ conn "create table if not exists decla(module_name text primary key, id text)"
    SQLite.close conn
  where
  db = outputDir </> "cache.db"
