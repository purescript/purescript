-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  ) where

import Prelude.Compat

import Data.Aeson
import Data.Version (Version, showVersion)
import Data.Text (pack)

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.Names

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

qualifiedToJSON :: (a -> String) -> Qualified a -> Value
qualifiedToJSON f = toJSON . showQualified f

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON = toJSON . runModuleName

moduleToJSON :: Version -> Module a -> Value
moduleToJSON v m = object [ pack "imports"   .= map (moduleNameToJSON . snd) (moduleImports m)
                          , pack "exports"   .= map identToJSON (moduleExports m)
                          , pack "foreign"   .= map (identToJSON . fst) (moduleForeign m)
                          , pack "decls"     .= map bindToJSON (moduleDecls m)
                          , pack "builtWith" .= toJSON (showVersion v)
                          ]

bindToJSON :: Bind a -> Value
bindToJSON (NonRec _ n e) = object [ pack (runIdent n) .= exprToJSON e ]
bindToJSON (Rec bs) = object $ map (\((_, n), e) -> pack (runIdent n) .= exprToJSON e) bs

recordToJSON :: (a -> Value) -> [(String, a)] -> Value
recordToJSON f = object . map (\(label, a) -> pack label .= f a)

exprToJSON :: Expr a -> Value
exprToJSON (Var _ i)              = toJSON ( "Var"
                                           , qualifiedToJSON runIdent i
                                           )
exprToJSON (Literal _ l)          = toJSON ( "Literal"
                                           , literalToJSON (exprToJSON) l
                                           )
exprToJSON (Constructor _ d c is) = toJSON ( "Constructor"
                                           , properNameToJSON d
                                           , properNameToJSON c
                                           , map identToJSON is
                                           )
exprToJSON (Accessor _ f r)       = toJSON ( "Accessor"
                                           , f
                                           , exprToJSON r
                                           )
exprToJSON (ObjectUpdate _ r fs)  = toJSON ( "ObjectUpdate"
                                           , exprToJSON r
                                           , recordToJSON exprToJSON fs
                                           )
exprToJSON (Abs _ p b)            = toJSON ( "Abs"
                                           , identToJSON p
                                           , exprToJSON b
                                           )
exprToJSON (App _ f x)            = toJSON ( "App"
                                           , exprToJSON f
                                           , exprToJSON x
                                           )
exprToJSON (Case _ ss cs)         = toJSON ( "Case"
                                           , map exprToJSON ss
                                           , map caseAlternativeToJSON cs
                                           )
exprToJSON (Let _ bs e)           = toJSON ( "Let"
                                           , map bindToJSON bs
                                           , exprToJSON e
                                           )

caseAlternativeToJSON :: CaseAlternative a -> Value
caseAlternativeToJSON (CaseAlternative bs r') =
  toJSON [ toJSON (map binderToJSON bs)
         , case r' of
             Left rs -> toJSON $ map (\(g, e) -> (exprToJSON g, exprToJSON e)) rs
             Right r -> exprToJSON r
         ]

binderToJSON :: Binder a -> Value
binderToJSON (VarBinder _ v)              = toJSON ( "VarBinder"
                                                   , identToJSON v
                                                   )
binderToJSON (NullBinder _)               = toJSON "NullBinder"
binderToJSON (LiteralBinder _ l)          = toJSON ( "LiteralBinder"
                                                   , literalToJSON binderToJSON l
                                                   )
binderToJSON (ConstructorBinder _ d c bs) = toJSON ( "ConstructorBinder"
                                                   , qualifiedToJSON runProperName d
                                                   , qualifiedToJSON runProperName c
                                                   , map binderToJSON bs
                                                   )
binderToJSON (NamedBinder _ n b)          = toJSON ( "NamedBinder"
                                                   , identToJSON n
                                                   , binderToJSON b
                                                   )
