{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  ) where

import Prelude.Compat

import Control.Arrow ((***))
import Data.Maybe (fromMaybe, maybe)
import Data.Aeson
import Data.Version (Version, showVersion)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString)

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON _ (NumericLiteral (Left n)) = toJSON ("IntLiteral", n)
literalToJSON _ (NumericLiteral (Right n)) = toJSON ("NumberLiteral", n)
literalToJSON _ (StringLiteral s) = toJSON ("StringLiteral", s)
literalToJSON _ (CharLiteral c) = toJSON ("CharLiteral", c)
literalToJSON _ (BooleanLiteral b) = toJSON ("BooleanLiteral", b)
literalToJSON t (ArrayLiteral xs) = toJSON ("ArrayLiteral", map t xs)
literalToJSON t (ObjectLiteral xs) = toJSON ("ObjectLiteral", recordToJSON t xs)

constructorTypeToJSON :: ConstructorType -> Value
constructorTypeToJSON ProductType = toJSON "ProductType"
constructorTypeToJSON SumType = toJSON "SumType"

metaToJSON :: Meta -> Value
metaToJSON (IsConstructor t is)   = toJSON ( "IsConstructor", constructorTypeToJSON t, toJSON (map runIdent is) )
metaToJSON IsNewtype              = toJSON "IsNewtype"
metaToJSON IsTypeClassConstructor = toJSON "IsTypeClassConstructor"
metaToJSON IsForeign              = toJSON "IsForeign"

annToJSON :: Ann -> Value
annToJSON (ss, _, _, m) = toJSON ( "Ann", toJSON ss, maybe Null metaToJSON m )

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

qualifiedToJSON :: (a -> Text) -> Qualified a -> Value
qualifiedToJSON f = toJSON . showQualified f

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON = toJSON . runModuleName

moduleToJSON :: Version -> ModuleT a Ann -> Value
moduleToJSON v m = object [ T.pack "imports"   .= map (annToJSON *** moduleNameToJSON) (moduleImports m)
                          , T.pack "exports"   .= map identToJSON (moduleExports m)
                          , T.pack "foreign"   .= map (identToJSON . fst) (moduleForeign m)
                          , T.pack "decls"     .= map bindToJSON (moduleDecls m)
                          , T.pack "builtWith" .= toJSON (showVersion v)
                          ]

bindToJSON :: Bind Ann -> Value
bindToJSON (NonRec ann n e) = toJSON [(runIdent n, annToJSON ann, exprToJSON e)]
bindToJSON (Rec bs) = toJSON $ map (\((ann, n), e) -> (runIdent n, annToJSON ann, exprToJSON e)) bs

-- If all of the labels in the record can safely be converted to JSON strings,
-- we generate a JSON object. Otherwise the labels must be represented as
-- arrays of integers in the JSON, and in this case we generate the record as
-- an array of pairs.
recordToJSON :: (a -> Value) -> [(PSString, a)] -> Value
recordToJSON f rec = fromMaybe (asArrayOfPairs rec) (asObject rec)
  where
  asObject = fmap object . traverse (uncurry maybePair)
  maybePair label a = fmap (\l -> l .= f a) (decodeString label)

  asArrayOfPairs = toJSON . map (\(label, a) -> (toJSON label, f a))

exprToJSON :: Expr Ann -> Value
exprToJSON (Var ann i)              = toJSON ( "Var"
                                             , annToJSON ann
                                             , qualifiedToJSON runIdent i
                                             )
exprToJSON (Literal ann l)          = toJSON ( "Literal"
                                             , annToJSON ann
                                             , literalToJSON (exprToJSON) l
                                             )
exprToJSON (Constructor ann d c is) = toJSON ( "Constructor"
                                             , annToJSON ann
                                             , properNameToJSON d
                                             , properNameToJSON c
                                             , map identToJSON is
                                             )
exprToJSON (Accessor ann f r)       = toJSON ( "Accessor"
                                             , annToJSON ann
                                             , f
                                             , exprToJSON r
                                             )
exprToJSON (ObjectUpdate ann r fs)  = toJSON ( "ObjectUpdate"
                                             , annToJSON ann
                                             , exprToJSON r
                                             , recordToJSON exprToJSON fs
                                             )
exprToJSON (Abs ann p b)            = toJSON ( "Abs"
                                             , annToJSON ann
                                             , identToJSON p
                                             , exprToJSON b
                                             )
exprToJSON (App ann f x)            = toJSON ( "App"
                                             , annToJSON ann
                                             , exprToJSON f
                                             , exprToJSON x
                                             )
exprToJSON (Case ann ss cs)         = toJSON ( "Case"
                                             , annToJSON ann
                                             , map exprToJSON ss
                                             , map caseAlternativeToJSON cs
                                             )
exprToJSON (Let ann bs e)           = toJSON ( "Let"
                                             , annToJSON ann
                                             , map bindToJSON bs
                                             , exprToJSON e
                                             )

caseAlternativeToJSON :: CaseAlternative Ann -> Value
caseAlternativeToJSON (CaseAlternative bs r') =
  toJSON [ toJSON (map binderToJSON bs)
         , case r' of
             Left rs -> toJSON $ map (\(g, e) -> (exprToJSON g, exprToJSON e)) rs
             Right r -> exprToJSON r
         ]

binderToJSON :: Binder Ann -> Value
binderToJSON (VarBinder ann v)              = toJSON ( "VarBinder"
                                                     , annToJSON ann
                                                     , identToJSON v
                                                     )
binderToJSON (NullBinder ann)               = toJSON ( "NullBinder"
                                                     , annToJSON ann
                                                     )
binderToJSON (LiteralBinder ann l)          = toJSON ( "LiteralBinder"
                                                     , annToJSON ann
                                                     , literalToJSON binderToJSON l
                                                     )
binderToJSON (ConstructorBinder ann d c bs) = toJSON ( "ConstructorBinder"
                                                     , annToJSON ann
                                                     , qualifiedToJSON runProperName d
                                                     , qualifiedToJSON runProperName c
                                                     , map binderToJSON bs
                                                     )
binderToJSON (NamedBinder ann n b)          = toJSON ( "NamedBinder"
                                                     , annToJSON ann
                                                     , identToJSON n
                                                     , binderToJSON b
                                                     )
