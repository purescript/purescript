{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Language.PureScript.Ide.Types where

import           Prelude                              ()
import           Prelude.Compat

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Map.Lazy                        as M
import           Data.Maybe                           (maybeToList)
import           Data.Text                            (Text (), pack, unpack)
import qualified Language.PureScript.AST.Declarations as D
import           Language.PureScript.Externs
import           Language.PureScript.Names
import qualified Language.PureScript.Names            as N

import           Text.Parsec
import           Text.Parsec.Text

type ModuleIdent = Text
type DeclIdent   = Text
type Type        = Text

data Fixity = Infix | Infixl | Infixr deriving(Show, Eq, Ord)

data ExternDecl
    = FunctionDecl { functionName :: DeclIdent
                   , functionType :: Type
                   }
    | FixityDeclaration Fixity
                        Int
                        DeclIdent
    | Dependency { dependencyModule :: ModuleIdent
                 , dependencyNames  :: [Text]
                 , dependencyAlias  :: Maybe Text
                 }
    | ModuleDecl ModuleIdent
                 [DeclIdent]
    | DataDecl DeclIdent
               Text
    | Export ModuleIdent
    deriving (Show,Eq,Ord)

instance ToJSON ExternDecl where
  toJSON (FunctionDecl n t)        = object ["name" .= n, "type" .= t]
  toJSON (ModuleDecl   n t)        = object ["name" .= n, "type" .= t]
  toJSON (DataDecl     n t)        = object ["name" .= n, "type" .= t]
  toJSON (Dependency   n names _)  = object ["module" .= n, "names" .= names]
  toJSON (FixityDeclaration f p n) = object ["name" .= n
                                            , "fixity" .= show f
                                            , "precedence" .= p]
  toJSON (Export _) = object []

type Module = (ModuleIdent, [ExternDecl])

data Configuration =
  Configuration {
    confOutputPath :: FilePath
  , confDebug      :: Bool
  }

data PscIdeEnvironment =
  PscIdeEnvironment {
    envStateVar      :: TVar PscIdeState
  , envConfiguration :: Configuration
  }

type PscIde m = (Applicative m, MonadIO m, MonadReader PscIdeEnvironment m)

data PscIdeState =
  PscIdeState {
    pscStateModules :: M.Map Text [ExternDecl]
  , externsFiles    :: M.Map ModuleName ExternsFile
  } deriving Show

emptyPscIdeState :: PscIdeState
emptyPscIdeState = PscIdeState M.empty M.empty

newtype Completion =
    Completion (ModuleIdent, DeclIdent, Type)
    deriving (Show,Eq)

data ModuleImport =
  ModuleImport {
    importModuleName :: ModuleIdent
  , importType       :: D.ImportDeclarationType
  , importQualifier  :: Maybe Text
  } deriving(Show)

instance Eq ModuleImport where
  mi1 == mi2 = importModuleName mi1 == importModuleName mi2
               && importQualifier mi1 == importQualifier mi2

instance ToJSON ModuleImport where
  toJSON (ModuleImport mn D.Implicit qualifier) =
    object $  ["module" .= mn
              , "importType" .= ("implicit" :: Text)
              ] ++ fmap (\x -> "qualifier" .= x) (maybeToList qualifier)
  toJSON (ModuleImport mn (D.Explicit refs) _) =
    object ["module" .= mn
           , "importType" .= ("explicit" :: Text)
           , "identifiers" .= (identifierFromDeclarationRef <$> refs)]
  toJSON (ModuleImport mn (D.Hiding refs) _) =
    object ["module" .= mn
           , "importType" .= ("hiding" :: Text)
           , "identifiers" .= (identifierFromDeclarationRef <$> refs)]

identifierFromDeclarationRef :: D.DeclarationRef -> String
identifierFromDeclarationRef (D.TypeRef name _) = N.runProperName name
identifierFromDeclarationRef (D.ValueRef ident) = N.runIdent ident
identifierFromDeclarationRef (D.TypeClassRef name) = N.runProperName name
identifierFromDeclarationRef _ = ""

instance FromJSON Completion where
    parseJSON (Object o) = do
        m <- o .: "module"
        d <- o .: "identifier"
        t <- o .: "type"
        return $ Completion (m, d, t)
    parseJSON _ = mzero

instance ToJSON Completion where
    toJSON (Completion (m,d,t)) =
        object ["module" .= m, "identifier" .= d, "type" .= t]

data Success =
  CompletionResult [Completion]
  | TextResult Text
  | MultilineTextResult [Text]
  | PursuitResult [PursuitResponse]
  | ImportList [ModuleImport]
  | ModuleList [ModuleIdent]
  deriving(Show, Eq)

encodeSuccess :: (ToJSON a) => a -> Value
encodeSuccess res =
    object ["resultType" .= ("success" :: Text), "result" .= res]

instance ToJSON Success where
  toJSON (CompletionResult cs) = encodeSuccess cs
  toJSON (TextResult t) = encodeSuccess t
  toJSON (MultilineTextResult ts) = encodeSuccess ts
  toJSON (PursuitResult resp) = encodeSuccess resp
  toJSON (ImportList decls) = encodeSuccess decls
  toJSON (ModuleList modules) = encodeSuccess modules

newtype PursuitQuery = PursuitQuery Text
                     deriving (Show, Eq)

data PursuitSearchType = Package | Identifier
                       deriving (Show, Eq)

instance FromJSON PursuitSearchType where
  parseJSON (String t) = case t of
    "package"    -> return Package
    "completion" -> return Identifier
    _            -> mzero
  parseJSON _ = mzero

instance FromJSON PursuitQuery where
  parseJSON o = fmap PursuitQuery (parseJSON o)

data PursuitResponse
    = ModuleResponse { moduleResponseName    :: Text
                     , moduleResponsePackage :: Text}
    | DeclarationResponse { declarationResponseType    :: Text
                          , declarationResponseModule  :: Text
                          , declarationResponseIdent   :: Text
                          , declarationResponsePackage :: Text
                          }
    deriving (Show,Eq)

instance FromJSON PursuitResponse where
  parseJSON (Object o) = do
    package <- o .: "package"
    info <- o .: "info"
    (type' :: String) <- info .: "type"
    case type' of
      "module" -> do
          name <- info .: "module"
          return
              ModuleResponse
              { moduleResponseName = name
              , moduleResponsePackage = package
              }
      "declaration" -> do
          moduleName <- info .: "module"
          Right (ident, declType) <- typeParse <$> o .: "text"
          return
              DeclarationResponse
              { declarationResponseType = declType
              , declarationResponseModule = moduleName
              , declarationResponseIdent = ident
              , declarationResponsePackage = package
              }
      _ -> mzero
  parseJSON _ = mzero


typeParse :: Text -> Either Text (Text, Text)
typeParse t = case parse parseType "" t of
  Right (x,y) -> Right (pack x, pack y)
  Left err -> Left (pack (show err))
  where
    parseType :: Parser (String, String)
    parseType = do
      name <- identifier
      _ <- string "::"
      spaces
      type' <- many1 anyChar
      return (unpack name, type')

identifier :: Parser Text
identifier = do
  spaces
  ident <-
    -- necessary for being able to parse the following ((++), concat)
    between (char '(') (char ')') (many1 (noneOf ", )")) <|>
    many1 (noneOf ", )")
  spaces
  return (pack ident)

instance ToJSON PursuitResponse where
  toJSON ModuleResponse{..} =
    object ["module" .= moduleResponseName, "package" .= moduleResponsePackage]
  toJSON DeclarationResponse{..} =
    object
      [ "module"  .= declarationResponseModule
      , "ident"   .= declarationResponseIdent
      , "type"    .= declarationResponseType
      , "package" .= declarationResponsePackage]
