{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Data types for names
--
module Language.PureScript.Names where

import Control.Monad (liftM)
import Control.Monad.Supply.Class

import Data.List
import Data.Aeson
import Data.Aeson.TH

-- |
-- Names for value identifiers
--
data Ident
  -- |
  -- An alphanumeric identifier
  --
  = Ident String
  -- |
  -- A symbolic name for an infix operator
  --
  | Op String
  -- |
  -- A generated name for an identifier
  --
  | GenIdent (Maybe String) Integer
  deriving (Show, Read, Eq, Ord)

runIdent :: Ident -> String
runIdent (Ident i) = i
runIdent (Op op) = op
runIdent (GenIdent Nothing n) = "$" ++ show n
runIdent (GenIdent (Just name) n) = "$" ++ name ++ show n

showIdent :: Ident -> String
showIdent (Op op) = '(' : op ++ ")"
showIdent i = runIdent i

freshIdent :: (MonadSupply m) => String -> m Ident
freshIdent name = liftM (GenIdent (Just name)) fresh

freshIdent' :: (MonadSupply m) => m Ident
freshIdent' = liftM (GenIdent Nothing) fresh

-- |
-- Proper names, i.e. capitalized names for e.g. module names, type//data constructors.
--
newtype ProperName (a :: ProperNameType) = ProperName { runProperName :: String }
  deriving (Show, Read, Eq, Ord)

instance ToJSON (ProperName a) where
  toJSON = toJSON . runProperName

instance FromJSON (ProperName a) where
  parseJSON = fmap ProperName . parseJSON

-- |
-- The closed set of proper name types.
--
data ProperNameType = TypeName | ConstructorName | ClassName | Namespace

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
  deriving (Show, Read, Eq, Ord)

runModuleName :: ModuleName -> String
runModuleName (ModuleName pns) = intercalate "." (runProperName `map` pns)

moduleNameFromString :: String -> ModuleName
moduleNameFromString = ModuleName . splitProperNames
  where
  splitProperNames s = case dropWhile (== '.') s of
    "" -> []
    s' -> ProperName w : splitProperNames s''
      where (w, s'') = break (== '.') s'

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a
  deriving (Show, Read, Eq, Ord, Functor)

showQualified :: (a -> String) -> Qualified a -> String
showQualified f (Qualified Nothing a) = f a
showQualified f (Qualified (Just name) a) = runModuleName name ++ "." ++ f a

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
