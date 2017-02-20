{-# LANGUAGE TemplateHaskell #-}

-- |
-- Data types for names
--
module Language.PureScript.Names where

import Prelude.Compat

import Control.Monad.Supply.Class

import Data.Aeson
import Data.Aeson.TH
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

-- | A sum of the possible name types, useful for error and lint messages.
data Name
  = IdentName Ident
  | ValOpName (OpName 'ValueOpName)
  | TyName (ProperName 'TypeName)
  | TyOpName (OpName 'TypeOpName)
  | DctorName (ProperName 'ConstructorName)
  | TyClassName (ProperName 'ClassName)
  | ModName ModuleName
  | KiName (ProperName 'KindName)
  deriving (Eq, Ord, Show)

getIdentName :: Name -> Maybe Ident
getIdentName (IdentName name) = Just name
getIdentName _ = Nothing

getValOpName :: Name -> Maybe (OpName 'ValueOpName)
getValOpName (ValOpName name) = Just name
getValOpName _ = Nothing

getTypeName :: Name -> Maybe (ProperName 'TypeName)
getTypeName (TyName name) = Just name
getTypeName _ = Nothing

getTypeOpName :: Name -> Maybe (OpName 'TypeOpName)
getTypeOpName (TyOpName name) = Just name
getTypeOpName _ = Nothing

getDctorName :: Name -> Maybe (ProperName 'ConstructorName)
getDctorName (DctorName name) = Just name
getDctorName _ = Nothing

getClassName :: Name -> Maybe (ProperName 'ClassName)
getClassName (TyClassName name) = Just name
getClassName _ = Nothing

getModName :: Name -> Maybe ModuleName
getModName (ModName name) = Just name
getModName _ = Nothing

-- |
-- Names for value identifiers
--
data Ident
  -- |
  -- An alphanumeric identifier
  --
  = Ident Text
  -- |
  -- A generated name for an identifier
  --
  | GenIdent (Maybe Text) Integer
  deriving (Show, Eq, Ord)

runIdent :: Ident -> Text
runIdent (Ident i) = i
runIdent (GenIdent Nothing n) = "$" <> T.pack (show n)
runIdent (GenIdent (Just name) n) = "$" <> name <> T.pack (show n)

showIdent :: Ident -> Text
showIdent = runIdent

freshIdent :: MonadSupply m => Text -> m Ident
freshIdent name = GenIdent (Just name) <$> fresh

freshIdent' :: MonadSupply m => m Ident
freshIdent' = GenIdent Nothing <$> fresh

-- |
-- Operator alias names.
--
newtype OpName (a :: OpNameType) = OpName { runOpName :: Text }
  deriving (Show, Eq, Ord)

instance ToJSON (OpName a) where
  toJSON = toJSON . runOpName

instance FromJSON (OpName a) where
  parseJSON = fmap OpName . parseJSON

showOp :: OpName a -> Text
showOp op = "(" <> runOpName op <> ")"

-- |
-- The closed set of operator alias types.
--
data OpNameType = ValueOpName | TypeOpName

-- |
-- Proper names, i.e. capitalized names for e.g. module names, type//data constructors.
--
newtype ProperName (a :: ProperNameType) = ProperName { runProperName :: Text }
  deriving (Show, Eq, Ord)

instance ToJSON (ProperName a) where
  toJSON = toJSON . runProperName

instance FromJSON (ProperName a) where
  parseJSON = fmap ProperName . parseJSON

-- |
-- The closed set of proper name types.
--
data ProperNameType
  = TypeName
  | ConstructorName
  | ClassName
  | KindName
  | Namespace

-- |
-- Coerces a ProperName from one ProperNameType to another. This should be used
-- with care, and is primarily used to convert ClassNames into TypeNames after
-- classes have been desugared.
--
coerceProperName :: ProperName a -> ProperName b
coerceProperName = ProperName . runProperName

-- |
-- Module names
--
newtype ModuleName = ModuleName [ProperName 'Namespace]
  deriving (Show, Eq, Ord)

runModuleName :: ModuleName -> Text
runModuleName (ModuleName pns) = T.intercalate "." (runProperName <$> pns)

moduleNameFromString :: Text -> ModuleName
moduleNameFromString = ModuleName . splitProperNames
  where
  splitProperNames s = case T.dropWhile (== '.') s of
    "" -> []
    s' -> ProperName w : splitProperNames s''
      where (w, s'') = T.break (== '.') s'

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a
  deriving (Show, Eq, Ord, Functor)

showQualified :: (a -> Text) -> Qualified a -> Text
showQualified f (Qualified Nothing a) = f a
showQualified f (Qualified (Just name) a) = runModuleName name <> "." <> f a

getQual :: Qualified a -> Maybe ModuleName
getQual (Qualified mn _) = mn

-- |
-- Provide a default module name, if a name is unqualified
--
qualify :: ModuleName -> Qualified a -> (ModuleName, a)
qualify m (Qualified Nothing a) = (m, a)
qualify _ (Qualified (Just m) a) = (m, a)

-- |
-- Makes a qualified value from a name and module name.
--
mkQualified :: a -> ModuleName -> Qualified a
mkQualified name mn = Qualified (Just mn) name

-- | Remove the module name from a qualified name
disqualify :: Qualified a -> a
disqualify (Qualified _ a) = a

-- |
-- Remove the qualification from a value when it is qualified with a particular
-- module name.
--
disqualifyFor :: Maybe ModuleName -> Qualified a -> Maybe a
disqualifyFor mn (Qualified mn' a) | mn == mn' = Just a
disqualifyFor _ _ = Nothing

-- |
-- Checks whether a qualified value is actually qualified with a module reference
--
isQualified :: Qualified a -> Bool
isQualified (Qualified Nothing _) = False
isQualified _ = True

-- |
-- Checks whether a qualified value is not actually qualified with a module reference
--
isUnqualified :: Qualified a -> Bool
isUnqualified = not . isQualified

-- |
-- Checks whether a qualified value is qualified with a particular module
--
isQualifiedWith :: ModuleName -> Qualified a -> Bool
isQualifiedWith mn (Qualified (Just mn') _) = mn == mn'
isQualifiedWith _ _ = False

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Qualified)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Ident)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ModuleName)
