{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  ) where

import           Prelude.Compat

import           Control.Arrow ((***))
import           Data.Either (isLeft)
import           Data.Maybe (maybe)
import           Data.Aeson
import           Data.Version (Version, showVersion)
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.PureScript.AST.Literals
import           Language.PureScript.AST.SourcePos (SourceSpan(SourceSpan))
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Language.PureScript.PSString (PSString)

constructorTypeToJSON :: ConstructorType -> Value
constructorTypeToJSON ProductType = toJSON "ProductType"
constructorTypeToJSON SumType = toJSON "SumType"

metaToJSON :: Meta -> Value
metaToJSON (IsConstructor t is)
  = object
    [ T.pack "metaType"         .= "IsConstructor"
    , T.pack "constructorType"  .= constructorTypeToJSON t
    , T.pack "identifiers"      .= identToJSON `map` is
    ]
metaToJSON IsNewtype              = object [ T.pack "metaType"  .= "IsNewtype" ]
metaToJSON IsTypeClassConstructor = object [ T.pack "metaType"  .= "IsTypeClassConstructor" ]
metaToJSON IsForeign              = object [ T.pack "metaType"  .= "IsForeign" ]
metaToJSON IsWhere                = object [ T.pack "metaType"  .= "IsWhere" ]

sourceSpanToJSON :: SourceSpan -> Value
sourceSpanToJSON (SourceSpan _ spanStart spanEnd) =
  object [ T.pack "start" .= spanStart
         , T.pack "end"   .= spanEnd
         ]

annToJSON :: Ann -> Value
annToJSON (ss, _, _, m) = object [ T.pack "sourceSpan"  .= sourceSpanToJSON ss
                                 , T.pack "meta"        .= maybe Null metaToJSON m
                                 ]

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON _ (NumericLiteral (Left n))
  = object
    [ T.pack "literalType" .= "IntLiteral"
    , T.pack "value"       .= n
    ]
literalToJSON _ (NumericLiteral (Right n))
  = object
      [ T.pack "literalType"  .= "NumberLiteral"
      , T.pack "value"        .= n
      ]
literalToJSON _ (StringLiteral s)
  = object
    [ T.pack "literalType"  .= "StringLiteral"
    , T.pack "value"        .= s
    ]
literalToJSON _ (CharLiteral c)
  = object
    [ T.pack "literalType"  .= "CharLiteral"
    , T.pack "value"        .= c
    ]
literalToJSON _ (BooleanLiteral b)
  = object
    [ T.pack "literalType"  .= "BooleanLiteral"
    , T.pack "value"        .= b
    ]
literalToJSON t (ArrayLiteral xs)
  = object
    [ T.pack "literalType"  .= "ArrayLiteral"
    , T.pack "value"        .= map t xs
    ]
literalToJSON t (ObjectLiteral xs)
  = object
    [ T.pack "literalType"    .= "ObjectLiteral"
    , T.pack "value"          .= recordToJSON t xs
    ]

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

qualifiedToJSON :: (a -> Text) -> Qualified a -> Value
qualifiedToJSON f (Qualified mn a) = object
  [ T.pack "moduleName"   .= maybe Null moduleNameToJSON mn
  , T.pack "identifier"   .= toJSON (f a)
  ]

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON (ModuleName name) = toJSON (T.splitOn (T.pack ".") name)

moduleToJSON :: Version -> Module Ann -> Value
moduleToJSON v m = object
  [ T.pack "sourceSpan" .= sourceSpanToJSON (moduleSourceSpan m)
  , T.pack "moduleName" .= moduleNameToJSON (moduleName m)
  , T.pack "modulePath" .= toJSON (modulePath m)
  , T.pack "imports"    .= map importToJSON (moduleImports m)
  , T.pack "exports"    .= map identToJSON (moduleExports m)
  , T.pack "foreign"    .= map identToJSON (moduleForeign m)
  , T.pack "decls"      .= map bindToJSON (moduleDecls m)
  , T.pack "builtWith"  .= toJSON (showVersion v)
  , T.pack "comments"   .= map toJSON (moduleComments m)
  ]

  where
  importToJSON (ann,mn) = object
    [ T.pack "annotation" .= annToJSON ann
    , T.pack "moduleName" .= moduleNameToJSON mn
    ]

bindToJSON :: Bind Ann -> Value
bindToJSON (NonRec ann n e)
  = object
    [ T.pack "bindType"   .= "NonRec"
    , T.pack "annotation" .= annToJSON ann
    , T.pack "identifier" .= identToJSON n
    , T.pack "expression" .= exprToJSON e
    ]
bindToJSON (Rec bs)
  = object
    [ T.pack "bindType"   .= "Rec"
    , T.pack "binds"      .= map (\((ann, n), e)
                                  -> object
                                      [ T.pack "identifier"  .= identToJSON n
                                      , T.pack "annotation"   .= annToJSON ann
                                      , T.pack "expression"   .= exprToJSON e
                                      ]) bs
    ]

recordToJSON :: (a -> Value) -> [(PSString, a)] -> Value
recordToJSON f = toJSON . map (toJSON *** f)

exprToJSON :: Expr Ann -> Value
exprToJSON (Var ann i)              = object [ T.pack "type"        .= toJSON "Var"
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "value"       .= qualifiedToJSON runIdent i
                                             ]
exprToJSON (Literal ann l)          = object [ T.pack "type"        .= "Literal"
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "value"       .=  literalToJSON exprToJSON l
                                             ]
exprToJSON (Constructor ann d c is) = object [ T.pack "type"        .= "Constructor"
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "typeName"    .= properNameToJSON d
                                             , T.pack "constructorName" .= properNameToJSON c
                                             , T.pack "fieldNames"  .= map identToJSON is
                                             ]
exprToJSON (Accessor ann f r)       = object [ T.pack "type"        .= "Accessor"
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "fieldName"   .= f
                                             , T.pack "expression"  .= exprToJSON r
                                             ]
exprToJSON (ObjectUpdate ann r fs)  = object [ T.pack "type"        .= "ObjectUpdate"
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "expression"  .= exprToJSON r
                                             , T.pack "updates"     .= recordToJSON exprToJSON fs
                                             ]
exprToJSON (Abs ann p b)            = object [ T.pack "type"        .= "Abs"
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "argument"    .= identToJSON p
                                             , T.pack "body"        .= exprToJSON b
                                             ]
exprToJSON (App ann f x)            = object [ T.pack "type"        .= "App"
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "abstraction" .= exprToJSON f
                                             , T.pack "argument"    .= exprToJSON x
                                             ]
exprToJSON (Case ann ss cs)         = object [ T.pack "type"        .= "Case"
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "caseExpressions"
                                                                    .= map exprToJSON ss
                                             , T.pack "caseAlternatives"
                                                                    .= map caseAlternativeToJSON cs
                                             ]
exprToJSON (Let ann bs e)           = object [ T.pack "type"        .= "Let" 
                                             , T.pack "annotation"  .= annToJSON ann
                                             , T.pack "binds"       .= map bindToJSON bs
                                             , T.pack "expression"  .= exprToJSON e
                                             ]

caseAlternativeToJSON :: CaseAlternative Ann -> Value
caseAlternativeToJSON (CaseAlternative bs r') =
  let isGuarded = isLeft r'
  in object
      [ T.pack "binders"     .= toJSON (map binderToJSON bs)
      , T.pack "isGuarded"   .= toJSON isGuarded
      , T.pack (if isGuarded then "expressions" else "expression")
         .= case r' of
             Left rs -> toJSON $ map (\(g, e) -> object [ T.pack "guard" .= exprToJSON g, T.pack "expression" .= exprToJSON e]) rs
             Right r -> exprToJSON r
      ]

binderToJSON :: Binder Ann -> Value
binderToJSON (VarBinder ann v)              = object [ T.pack "binderType"  .= "VarBinder"
                                                     , T.pack "annotation"  .= annToJSON ann
                                                     , T.pack "identifier"  .= identToJSON v
                                                     ]
binderToJSON (NullBinder ann)               = object [ T.pack "binderType"  .= "NullBinder"
                                                     , T.pack "annotation"  .= annToJSON ann
                                                     ]
binderToJSON (LiteralBinder ann l)          = object [ T.pack "binderType"  .= "LiteralBinder"
                                                     , T.pack "annotation"  .= annToJSON ann
                                                     , T.pack "literal"     .= literalToJSON binderToJSON l
                                                     ]
binderToJSON (ConstructorBinder ann d c bs) = object [ T.pack "binderType"  .= "ConstructorBinder"
                                                     , T.pack "annotation"  .= annToJSON ann
                                                     , T.pack "typeName"    .= qualifiedToJSON runProperName d
                                                     , T.pack "constructorName"
                                                                            .= qualifiedToJSON runProperName c
                                                     , T.pack "binders"     .= map binderToJSON bs
                                                     ]
binderToJSON (NamedBinder ann n b)          = object [ T.pack "binderType"  .= "NamedBinder"
                                                     , T.pack "annotation"  .= annToJSON ann
                                                     , T.pack "identifier"  .= identToJSON n
                                                     , T.pack "binder"      .= binderToJSON b
                                                     ]
