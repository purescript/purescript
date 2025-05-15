module Language.PureScript.Make.IdeCache where

import Prelude

import Language.PureScript.Ide.ToIde (toIdeDeclarationAnn)
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple qualified as SQLite
import Codec.Serialise qualified as Serialise
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import System.FilePath ((</>), takeDirectory)
import Language.PureScript.Names (runModuleName, ProperName (runProperName), runIdent, disqualify, Ident (..), OpName (OpName))
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
import Language.PureScript.Ide.Util (identifierFromIdeDeclaration, discardAnn, namespaceForDeclaration)
import Data.Function ((&))
import Data.Bifunctor (first)
import Data.Text (Text)
import Language.PureScript.Ide.Types (Annotation(..), idaDeclaration, declarationType, IdeDeclarationAnn (_idaAnnotation), IdeNamespace (IdeNSValue, IdeNSType))
import Language.PureScript.Docs.Types (Declaration(declChildren))
import Language.PureScript.Docs.Render (renderDeclaration)
import Language.PureScript.Docs.AsMarkdown (codeToString, declAsMarkdown, runDocs)
import Codec.Serialise (serialise)
import Data.Aeson (encode)
import Debug.Trace qualified as Debug
import Language.PureScript.AST.Declarations (Module, Expr (Var), getModuleDeclarations, DeclarationRef (..), ExportSource (..))
import Language.PureScript.Ide.Filter.Declaration (DeclarationType (..))
import Data.Aeson qualified as Aeson
import Language.PureScript.AST.Traversals (everywhereOnValuesM)
import Protolude (identity)
import Language.PureScript.Names qualified as T

sqliteExtern :: (MonadIO m) => FilePath -> Module -> Docs.Module -> ExternsFile -> m ()
sqliteExtern outputDir m docs extern = liftIO $ do
    conn <- SQLite.open db
    SQLite.execute_ conn "pragma busy_timeout = 300000;"

    -- Debug.traceM $ show extern 

    let (doDecl, _, _) = everywhereOnValuesM (pure . identity) (\expr -> case expr of
         Var ss i -> do 
            let iv = disqualify i
            case iv of
              Ident t -> do
                SQLite.executeNamed conn
                  "insert into asts (module_name, name, span) values (:module_name, :name, :span)"
                  [ ":module_name" := runModuleName ( efModuleName extern )
                  , ":name" := t
                  , ":span" := Aeson.encode ss
                  ]
              _ -> pure ()
            pure expr
         _ -> pure expr
         ) (pure . identity)

    SQLite.execute_ conn "pragma foreign_keys = ON;"

    SQLite.executeNamed conn
      "delete from modules where module_name = :module_name"
      [ ":module_name" :=  runModuleName ( efModuleName extern )
      ]


    SQLite.executeNamed conn
      "insert into modules (module_name, comment, extern, dec) values (:module_name, :docs, :extern, :dec)"
      [ ":module_name" :=  runModuleName ( efModuleName extern )
      , ":docs" := Docs.modComments docs
      , ":extern" := Serialise.serialise extern
      , ":dec" := show ( efExports extern )
      ]

    for_ (getModuleDeclarations m) (\d -> doDecl d)

    for_ (efExports extern) (\case 
       ReExportRef _ (ExportSource _ definedIn) (ValueRef _ (Ident i)) -> do
         SQLite.executeNamed conn "insert into exports (module_name, name, defined_in, declaration_type) values (:module_name, :name, :defined_in, 'value')"
          [ ":module_name" := runModuleName (efModuleName extern )
          , ":name" := i
          , ":defined_in" := runModuleName definedIn
          ]
       ReExportRef _ (ExportSource _ definedIn) (ValueOpRef _ (OpName n)) -> do
         SQLite.executeNamed conn "insert into exports (module_name, name, defined_in, declaration_type) values (:module_name, :name, :defined_in, 'valueoperator')"
          [ ":module_name" := runModuleName (efModuleName extern )
          , ":name" := n
          , ":defined_in" := runModuleName definedIn
          ]
       ReExportRef _ (ExportSource _ definedIn) (TypeClassRef _ (T.ProperName n)) -> do
         SQLite.executeNamed conn "insert into exports (module_name, name, defined_in, declaration_type) values (:module_name, :name, :defined_in, 'typeclass')"
          [ ":module_name" := runModuleName (efModuleName extern )
          , ":name" := n
          , ":defined_in" := runModuleName definedIn
          ]
       _ -> pure ()
          )

    for_ (efImports extern) (\i -> do
       SQLite.executeNamed conn "insert into dependencies (module_name, dependency) values (:module_name, :dependency)"
        [ ":module_name" := runModuleName (efModuleName extern )
        , ":dependency" := runModuleName (eiModule i)
        ])

    for_ (toIdeDeclarationAnn m extern) (\ideDeclaration -> do
       SQLite.executeNamed conn
          ("insert into ide_declarations (module_name, name, namespace, declaration_type, span, declaration) " <>
           "values (:module_name, :name, :namespace, :declaration_type, :span, :declaration)"
          )
        [ ":module_name" := runModuleName (efModuleName extern )
        , ":name" := identifierFromIdeDeclaration (discardAnn ideDeclaration)
        , ":namespace" := namespaceForDeclaration (discardAnn ideDeclaration)
        , ":declaration_type" := declarationType (discardAnn ideDeclaration)
        , ":span" := serialise (_annLocation $ _idaAnnotation ideDeclaration)
        , ":declaration" := serialise ideDeclaration
        ])

    for_ (Docs.modDeclarations docs) (\d -> do
       SQLite.executeNamed conn
         ("insert into declarations (module_name, name, namespace, declaration_type, span, type, docs, declaration) " <>
          "values (:module_name, :name, :namespace, :declaration_type, :span, :type, :docs, :declaration)"
         )
        [ ":module_name" := runModuleName (efModuleName extern)
        , ":name" := Docs.declTitle d
        , ":namespace" := toIdeNamespace d
        , ":declaration_type" := toDeclarationType d
        , ":span" := Aeson.encode (Docs.declSourceSpan d)
        , ":docs" := Docs.declComments d
        , ":type" := runDocs (declAsMarkdown d)
        , ":declaration" := show d
        ]


       for_ (declChildren d) $ \ch -> do
         SQLite.executeNamed conn
            ("insert into declarations (module_name, name, namespace, span, docs, declaration) " <>
             "values (:module_name, :name, :namespace, :span, :docs, :declaration)")
          [ ":module_name" := runModuleName (efModuleName extern)
          , ":name" := Docs.cdeclTitle ch
          , ":namespace" := childDeclInfoNamespaceIde (Docs.cdeclInfo ch)
          , ":span" := Aeson.encode (Docs.declSourceSpan d)
          , ":docs" := Docs.cdeclComments ch
          , ":declaration" := show d
          ]
        )


    for_ (Docs.modReExports docs) $ \rexport -> do
       for_ (snd rexport) $ \d  -> do
         SQLite.executeNamed conn
           ("insert into declarations (module_name, name, rexported_from, declaration_type, span, type, docs, declaration)" <>
            "values (:module_name, :name, :rexported_from, :declaration_type, :span, :type, :docs, :declaration)"
           )
          [ ":module_name" := runModuleName (efModuleName extern)
          , ":name" := Docs.declTitle d
          , ":rexported_from" := ("HOLAS" :: Text) --runModuleName (Docs.ignorePackage (fst rexport))
          , ":declaration_type" := toDeclarationType d
          , ":span" := Aeson.encode (Docs.declSourceSpan d)
          , ":docs" := Docs.declComments d
          , ":type" := runDocs (declAsMarkdown d)
          , ":declaration" := show d
          ]

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

createParentDirectory :: FilePath -> IO ()
createParentDirectory = createDirectoryIfMissing True . takeDirectory

sqliteInit :: (MonadIO m) => FilePath -> m ()
sqliteInit outputDir = liftIO $ do
    createParentDirectory db
    conn <- SQLite.open db
    SQLite.execute_ conn "pragma busy_timeout = 300000;"
    SQLite.execute_ conn "pragma journal_mode=wal;"
    SQLite.execute_ conn "pragma foreign_keys = ON;"
    SQLite.execute_ conn $ SQLite.Query $ Text.pack $ unlines
      [ "create table if not exists modules ("
      , " module_name text primary key,"
      , " comment text,"
      , " extern blob,"
      , " dec text,"
      , " unique (module_name) on conflict replace"
      , ")"
      ]

    SQLite.execute_ conn $ SQLite.Query $ Text.pack $ unlines
      [ "create table if not exists dependencies ("
      , " module_name text not null references modules(module_name) on delete cascade,"
      , " dependency text not null,"
      , " unique (module_name, dependency) on conflict ignore"
      , ")"
      ]

    SQLite.execute_ conn $ SQLite.Query $ Text.pack $ unlines
      [ "create table if not exists declarations ("
      , " module_name text references modules(module_name) on delete cascade,"
      , " name text not null,"
      , " namespace text,"
      , " declaration_type text,"
      , " rexported_from text,"
      , " type text,"
      , " docs text,"
      , " span text,"
      , " declaration text not null"
      , ")"
      ]

    SQLite.execute_ conn $ SQLite.Query $ Text.pack $ unlines
      [ "create table if not exists asts ("
      , " module_name text references modules(module_name) on delete cascade,"
      , " name text not null,"
      , " span text"
      , ")"
      ]

    SQLite.execute_ conn $ SQLite.Query $ Text.pack $ unlines
      [ "create table if not exists exports ("
      , "module_name text references modules(module_name) on delete cascade,"
      , "name text not null,"
      , "defined_in text,"
      , "declaration_type text"
      , ")"
      ]

    SQLite.execute_ conn "create index if not exists dm on declarations(module_name)"
    SQLite.execute_ conn "create index if not exists dn on declarations(name);"
    
    SQLite.execute_ conn "create index if not exists asts_module_name_idx on asts(module_name);"
    SQLite.execute_ conn "create index if not exists asts_name_idx on asts(name);"


    SQLite.execute_ conn "create index if not exists exports_name_idx on exports(name);"
    SQLite.execute_ conn "create index if not exists exports_module_name_idx on exports(module_name);"

    SQLite.execute_ conn "create index if not exists exports_defined_in_id on exports(defined_in);"
    SQLite.execute_ conn "create index if not exists exports_declaration_type_idx on exports(declaration_type);"

    SQLite.execute_ conn "create table if not exists ide_declarations (module_name text references modules(module_name) on delete cascade, name text, namespace text, declaration_type text, span blob, declaration blob)"

    SQLite.execute_ conn "create index if not exists ide_declarations_name_idx on ide_declarations(name);"

    SQLite.execute_ conn "create index if not exists ide_declarations_module_name_idx on ide_declarations(module_name);"

    SQLite.execute_ conn "create index if not exists exports_idx on exports(defined_in,name,declaration_type,module_name);"

    SQLite.close conn
  where
  db = outputDir </> "cache.db"

toDeclarationType :: Declaration -> DeclarationType
toDeclarationType (Docs.Declaration _ _ _ _ (Docs.ValueDeclaration _) _) = Value
toDeclarationType (Docs.Declaration _ _ _ _ (Docs.DataDeclaration _ _ _) _) = Type
toDeclarationType (Docs.Declaration _ _ _ _ _ _ )  = Value

toIdeN :: Docs.Namespace -> IdeNamespace
toIdeN Docs.ValueLevel = IdeNSValue
toIdeN Docs.TypeLevel = IdeNSType

toIdeNamespace :: Declaration -> IdeNamespace
toIdeNamespace (Docs.Declaration _ _ _ _ declInfo _) = case Docs.declInfoNamespace declInfo of
  Docs.ValueLevel -> IdeNSValue
  Docs.TypeLevel -> IdeNSType

childDeclInfoNamespaceIde :: Docs.ChildDeclarationInfo -> IdeNamespace
childDeclInfoNamespaceIde = toIdeN . Docs.childDeclInfoNamespace
