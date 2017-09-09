{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  ) where

import Prelude.Compat

import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Version (Version, showVersion)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, decodeString)
import Language.PureScript.Kinds (Kind(..))
import Language.PureScript.Names (Qualified(..))
import Language.PureScript.Types (Type(..), Constraint(..))

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON _ (NumericLiteral (Left n)) = toJSON ("IntLiteral", n)
literalToJSON _ (NumericLiteral (Right n)) = toJSON ("NumberLiteral", n)
literalToJSON _ (StringLiteral s) = toJSON ("StringLiteral", s)
literalToJSON _ (CharLiteral c) = toJSON ("CharLiteral", c)
literalToJSON _ (BooleanLiteral b) = toJSON ("BooleanLiteral", b)
literalToJSON t (ArrayLiteral xs) = toJSON ("ArrayLiteral", map t xs)
literalToJSON t (ObjectLiteral xs) = toJSON ("ObjectLiteral", recordToJSON t xs)

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

qualifiedToJSON :: (a -> Text) -> Qualified a -> Value
qualifiedToJSON f = toJSON . showQualified f

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON = toJSON . runModuleName

moduleToJSON :: Version -> Module Ann -> Value
moduleToJSON v m = object [ T.pack "imports"   .= map (moduleNameToJSON . snd) (moduleImports m)
                          , T.pack "exports"   .= map identToJSON (moduleExports m)
                          , T.pack "foreign"   .= map (identToJSON . fst) (moduleForeign m)
                          , T.pack "decls"     .= map bindToJSON (moduleDecls m)
                          , T.pack "builtWith" .= toJSON (showVersion v)
                          ]

bindToJSON :: Bind Ann -> Value
bindToJSON (NonRec _ n e) = object [ runIdent n .= exprToJSON e ]
bindToJSON (Rec bs) = object $ map (\((_, n), e) -> runIdent n .= exprToJSON e) bs

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

constraintToJSON :: Constraint -> Value
constraintToJSON cnst = toJSON ( showQualified runProperName (constraintClass cnst)
                               , map typeToJSON (constraintArgs cnst)
                               )

kindToJSON :: Kind -> Value
kindToJSON (KUnknown _)  = Null
kindToJSON (Row k)       = toJSON ( "Row"
                                  , kindToJSON k
                                  )
kindToJSON (FunKind a x) = toJSON ( "FunKind"
                                  , kindToJSON a
                                  , kindToJSON x
                                  )
kindToJSON (NamedKind n) = toJSON ( "NamedKind"
                                  , showQualified runProperName n
                                  )

typeToJSON :: Type -> Value
typeToJSON (TUnknown _)            = Null
typeToJSON (TypeVar t)             = toJSON ( "Var"
                                            , toJSON t
                                            )
typeToJSON (TypeLevelString t)     = toJSON ( "TypeLevelString"
                                            , toJSON t
                                            )
typeToJSON (TypeWildcard _)        = toJSON ( "Wildcard" )
typeToJSON (TypeConstructor t)     = toJSON ( "Constructor"
                                            , showQualified runProperName t
                                            )
typeToJSON (TypeOp t)              = toJSON ( "Constructor"
                                            , showQualified runOpName t
                                            )
typeToJSON (TypeApp t a)           = toJSON ( "App"
                                            , typeToJSON t
                                            , typeToJSON a
                                            )
typeToJSON (ForAll a t _)          = toJSON ( "ForAll"
                                            , toJSON a
                                            , typeToJSON t
                                            )
typeToJSON (ConstrainedType cs ts) = toJSON ( "ConstrainedType"
                                            , map constraintToJSON cs
                                            , typeToJSON ts
                                            )
typeToJSON (Skolem t _ _ _)        = toJSON ( "Skolem"
                                            , toJSON t
                                            )
typeToJSON (REmpty)                = toJSON ( "REmpty" )
typeToJSON (RCons l a t)           = toJSON ( "RCons"
                                            , toJSON l
                                            , typeToJSON a
                                            , typeToJSON t
                                            )
typeToJSON (KindedType t k)        = toJSON ( "KindedType"
                                            , typeToJSON t
                                            , kindToJSON k
                                            )
typeToJSON _                       = Null

annToJSON :: Ann -> Value
annToJSON (_, _, Just t, _) = typeToJSON t
annToJSON _ = Null

exprToJSON :: Expr Ann -> Value
exprToJSON (Var a i)              = toJSON ( "Var"
                                           , qualifiedToJSON runIdent i
                                           , annToJSON a
                                           )
exprToJSON (Literal a l)          = toJSON ( "Literal"
                                           , literalToJSON (exprToJSON) l
                                           , annToJSON a
                                           )
exprToJSON (Constructor a d c is) = toJSON ( "Constructor"
                                           , properNameToJSON d
                                           , properNameToJSON c
                                           , map identToJSON is
                                           , annToJSON a
                                           )
exprToJSON (Accessor a f r)       = toJSON ( "Accessor"
                                           , f
                                           , exprToJSON r
                                           , annToJSON a
                                           )
exprToJSON (ObjectUpdate a r fs)  = toJSON ( "ObjectUpdate"
                                           , exprToJSON r
                                           , recordToJSON exprToJSON fs
                                           , annToJSON a
                                           )
exprToJSON (Abs a p b)            = toJSON ( "Abs"
                                           , identToJSON p
                                           , exprToJSON b
                                           , annToJSON a
                                           )
exprToJSON (App a f x)            = toJSON ( "App"
                                           , exprToJSON f
                                           , exprToJSON x
                                           , annToJSON a
                                           )
exprToJSON (Case a ss cs)         = toJSON ( "Case"
                                           , map exprToJSON ss
                                           , map caseAlternativeToJSON cs
                                           , annToJSON a
                                           )
exprToJSON (Let a bs e)           = toJSON ( "Let"
                                           , map bindToJSON bs
                                           , exprToJSON e
                                           , annToJSON a
                                           )

caseAlternativeToJSON :: CaseAlternative Ann -> Value
caseAlternativeToJSON (CaseAlternative bs r') =
  toJSON [ toJSON (map binderToJSON bs)
         , case r' of
             Left rs -> toJSON $ map (\(g, e) -> (exprToJSON g, exprToJSON e)) rs
             Right r -> exprToJSON r
         ]

binderToJSON :: Binder Ann -> Value
binderToJSON (VarBinder a v)              = toJSON ( "VarBinder"
                                                   , identToJSON v
                                                   , annToJSON a
                                                   )
binderToJSON (NullBinder _)               = toJSON "NullBinder"
binderToJSON (LiteralBinder a l)          = toJSON ( "LiteralBinder"
                                                   , literalToJSON binderToJSON l
                                                   , annToJSON a
                                                   )
binderToJSON (ConstructorBinder a d c bs) = toJSON ( "ConstructorBinder"
                                                   , qualifiedToJSON runProperName d
                                                   , qualifiedToJSON runProperName c
                                                   , map binderToJSON bs
                                                   , annToJSON a
                                                   )
binderToJSON (NamedBinder a n b)          = toJSON ( "NamedBinder"
                                                   , identToJSON n
                                                   , binderToJSON b
                                                   , annToJSON a
                                                   )
