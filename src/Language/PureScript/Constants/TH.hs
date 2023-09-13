{-# LANGUAGE TemplateHaskell #-}
-- | This module implements an eDSL for compactly declaring pattern synonyms
-- representing known PureScript modules and their members.
--
-- The following example assumes this module is imported qualified as TH and
-- the BlockArguments extension is used, both of which I recommend.
--
-- > $(TH.declare do
-- >   TH.mod "Data.Foo" do
-- >     TH.ty "SomeType"
-- >     TH.asIdent do
-- >       TH.var "someVariable"
-- >   )
--
-- will become:
--
-- > pattern M_Data_Foo :: ModuleName
-- > pattern M_Data_Foo = ModuleName "Data.Foo"
-- >
-- > pattern SomeType :: Qualified (ProperName 'TypeName)
-- > pattern SomeType = Qualified (ByModuleName M_Data_Foo) (ProperName "SomeType")
-- >
-- > pattern I_someVariable :: Qualified Ident
-- > pattern I_someVariable = Qualified (ByModuleName M_Data_Foo) (Ident "someVariable")
--
-- All pattern synonyms must start with an uppercase letter. To prevent
-- namespace collisions, different types of pattern are distinguished by a sort
-- of Hungarian notation convention:
--
-- @
--   SomeType   -- a type or class name
--   C_Ctor     -- a constructor name
--   I_name     -- a Qualified Ident
--   M_Data_Foo -- a module name
--   P_name     -- a (module name, polymorphic string) pair
--   S_name     -- a lone polymorphic string (this doesn't contain any module information)
-- @
--
-- I_, P_, and S_ patterns are all optional and have to be enabled with
-- `asIdent`, `asPair`, and `asString` modifiers respectively.
--
-- Finally, to disambiguate between identifiers with the same name (such as
-- Data.Function.apply and Data.Apply.apply), the `prefixWith` modifier will
-- modify the names of the patterns created within it.
--
-- > TH.mod "Data.Function" do
-- >   TH.prefixWith "function" do
-- >     TH.asIdent do
-- >       TH.var "apply"
-- 
-- results in:
--
-- > pattern I_functionApply :: Qualified Ident
-- > pattern I_functionApply = Qualified (ByModuleName (M_Data_Function) (Ident "apply")
--
module Language.PureScript.Constants.TH
  ( declare
  , mod
  , cls, clss
  , dty
  , nty, ntys
  , ty, tys
  , var, vars
  , prefixWith
  , asIdent
  , asPair
  , asString
  ) where

import Protolude hiding (Type, mod)

import Control.Lens (over, _head)
import Control.Monad.Trans.RWS (RWS, execRWS)
import Control.Monad.Trans.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)
import Data.String (String)
import Language.Haskell.TH (Dec, Name, Pat, Q, Type, conP, implBidir, litP, mkName, patSynD, patSynSigD, prefixPatSyn, stringL)
import Language.PureScript.Names (Ident(..), ModuleName(..), ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..))

-- | Generate pattern synonyms corresponding to the provided PureScript
-- declarations.
declare :: Writer (Q [Dec]) () -> Q [Dec]
declare = execWriter

-- | Declare a module.
mod :: String -> ModDecs -> Writer (Q [Dec]) ()
mod mnStr inner = do
  -- pattern M_Data_Foo :: ModuleName
  -- pattern M_Data_Foo = ModuleName "Data.Foo"
  let mn = mkModuleName mnStr
  tell $ typedPatSyn mn [t| ModuleName |] [p| ModuleName $(litP $ stringL mnStr) |]
  tell $ snd $ execRWS inner (mn, "", []) ()

-- | Declare a type class. The resulting pattern will use the name of the class
-- and have type `Qualified (ProperName 'ClassName)`.
cls :: String -> ModDecs
cls cn = ask >>= \(mn, prefix, _) -> tell $ mkPnPat [t| 'ClassName |] mn prefix cn

-- | Declare a list of type classes; shorthand for repeatedly calling `cls`.
clss :: [String] -> ModDecs
clss = traverse_ cls

-- | Declare a data type, given the name of the type and a list of constructor
-- names. A pattern will be created using the name of the type and have type
-- `Qualified (ProperName 'TypeName)`. A pattern will also be created for each
-- constructor prefixed with "C_", having type `Qualified (ProperName
-- 'ConstructorName)`.
dty :: String -> [String] -> ModDecs
dty dn ctors = ask >>= \(mn, prefix, _) -> do
  tell $ mkPnPat [t| 'TypeName |] mn prefix dn
  tell $ map fold $ traverse (mkPnPat [t| 'ConstructorName |] mn $ "C_" <> prefix) ctors

-- | Declare a data type with a singular constructor named the same as the
-- type, as is commonly the case with newtypes (but this does not require the
-- type to be a newtype in reality). Shorthand for calling `dty`.
nty :: String -> ModDecs
nty tn = dty tn [tn]

-- | Declare a list of data types with singular constructors; shorthand for
-- repeatedly calling `nty`, which itself is shorthand for `dty`.
ntys :: [String] -> ModDecs
ntys = traverse_ nty

-- | Declare a type. The resulting pattern will use the name of the type and have
-- type `Qualified (ProperName 'TypeName)`.
ty :: String -> ModDecs
ty tn = ask >>= \(mn, prefix, _) -> tell $ mkPnPat [t| 'TypeName |] mn prefix tn

-- | Declare a list of types; shorthand for repeatedly calling `ty`.
tys :: [String] -> ModDecs
tys = traverse_ ty

-- | Declare a variable, function, named instance, or generally a lower-case
-- value member of a module. The patterns created depend on which of `asPair`,
-- `asIdent`, or `asString` are used in the enclosing context.
var :: String -> ModDecs
var nm = ask >>= \(mn, prefix, vtds) -> tell $ foldMap (\f -> f mn prefix nm) vtds

-- | Declare a list of variables; shorthand for repeatedly calling `var`.
vars :: [String] -> ModDecs
vars = traverse_ var

-- | For every variable declared within, create a pattern synonym prefixed
-- with "P_" having type `forall a. (Eq a, IsString a) => (ModuleName, a)`.
asPair :: ModDecs -> ModDecs
asPair = local $ addToVars mkPairDec

-- | For every variable declared within, cerate a pattern synonym prefixed
-- with "I_" having type `Qualified Ident`.
asIdent :: ModDecs -> ModDecs
asIdent = local $ addToVars mkIdentDec

-- | For every variable declared within, cerate a pattern synonym prefixed
-- with "S_" having type `forall a. (Eq a, IsString a) => a`.
asString :: ModDecs -> ModDecs
asString = local $ addToVars mkStringDec

-- | Prefix the names of all enclosed declarations with the provided string, to
-- prevent collisions with other identifiers. For example,
-- `prefixWith "function"` would turn `I_apply` into `I_functionApply`, and
-- `C_Example` into `C_FunctionExample`.
prefixWith :: String -> ModDecs -> ModDecs
prefixWith = local . applyPrefix

-- Internals start here

type ModDecs = RWS (Name, String, [VarToDec]) (Q [Dec]) () ()
type VarToDec = Name -> String -> String -> Q [Dec]

addToVars :: VarToDec -> (a, b, [VarToDec]) -> (a, b, [VarToDec])
addToVars f (a, b, fs) = (a, b, f : fs)

applyPrefix :: String -> (a, String, c) -> (a, String, c)
applyPrefix prefix (a, prefix', c) = (a, camelAppend prefix' prefix, c)

cap :: String -> String
cap = over _head toUpper

camelAppend :: String -> String -> String
camelAppend l r = if null l then r else l <> cap r

-- "Data.Foo" -> M_Data_Foo
mkModuleName :: String -> Name
mkModuleName = mkName . ("M_" <>) . map (\case '.' -> '_'; other -> other)

-- "I_" -> "fn" -> "foo" -> I_fnFoo
-- "I_" -> ""   -> "foo" -> I_foo
mkPrefixedName :: String -> String -> String -> Name
mkPrefixedName tag prefix = mkName . (tag <>) . camelAppend prefix

-- 'TypeName -> M_Data_Foo -> "Function" -> "Foo" ->
--   pattern FunctionFoo :: Qualified (ProperName 'TypeName)
--   pattern FunctionFoo = Qualified (ByModuleName M_Data_Foo) (ProperName "Foo")
mkPnPat :: Q Type -> VarToDec
mkPnPat pnType mn prefix str = typedPatSyn (mkName $ cap prefix <> str)
  [t| Qualified (ProperName $pnType) |]
  [p| Qualified (ByModuleName $(conP mn [])) (ProperName $(litP $ stringL str)) |]

-- M_Data_Foo -> "function" -> "foo" ->
--   pattern I_functionFoo :: Qualified Ident
--   pattern I_functionFoo = Qualified (ByModuleName M_Data_Foo) (Ident "foo")
mkIdentDec :: VarToDec
mkIdentDec mn prefix str = typedPatSyn (mkPrefixedName "I_" prefix str)
  [t| Qualified Ident |]
  [p| Qualified (ByModuleName $(conP mn [])) (Ident $(litP $ stringL str)) |]

-- M_Data_Foo -> "function" -> "foo" ->
--   pattern P_functionFoo :: forall a. (Eq a, IsString a) => (ModuleName, a)
--   pattern P_functionFoo = (M_Data_Foo, "foo")
mkPairDec :: VarToDec
mkPairDec mn prefix str = typedPatSyn (mkPrefixedName "P_" prefix str)
  [t| forall a. (Eq a, IsString a) => (ModuleName, a) |]
  [p| ($(conP mn []), $(litP $ stringL str)) |]

-- _ -> "function" -> "foo" ->
--   pattern S_functionFoo :: forall a. (Eq a, IsString a) => a
--   pattern S_functionFoo = "foo"
mkStringDec :: VarToDec
mkStringDec _ prefix str = typedPatSyn (mkPrefixedName "S_" prefix str)
  [t| forall a. (Eq a, IsString a) => a |]
  (litP $ stringL str)

typedPatSyn :: Name -> Q Type -> Q Pat -> Q [Dec]
typedPatSyn nm t p = sequence [patSynSigD nm t, patSynD nm (prefixPatSyn []) implBidir p]
