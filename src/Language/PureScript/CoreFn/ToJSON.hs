-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  , annToJSON
  ) where

import Prelude.Compat

import Data.Aeson
import Data.Text (pack)

import Language.PureScript.AST.Literals
import Language.PureScript.Comments
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Types

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON _ (NumericLiteral (Left n)) = toJSON ("NumericLiteral", "Int", n)
literalToJSON _ (NumericLiteral (Right n)) = toJSON ("NumericLiteral", "Number", n)
literalToJSON _ (StringLiteral s) = toJSON ("StringLiteral", s)
literalToJSON _ (CharLiteral c) = toJSON ("CharLiteral", c)
literalToJSON _ (BooleanLiteral b) = toJSON ("BooleanLiteral", b)
literalToJSON t (ArrayLiteral xs) = toJSON ("ArrayLiteral", map t xs)
literalToJSON t (ObjectLiteral xs) = toJSON ("ObjectLiteral", map (fmap t) xs)

annToJSON :: Ann -> Value
annToJSON (_ss, cs, _t, m) = toJSON ( Null -- sourceSpanToJSON <$> ss
                                    , map commentToJSON cs
                                    , Null -- typeToJSON <$> t
                                    , metaToJSON <$> m
                                    )

metaToJSON :: Meta -> Value
metaToJSON (IsConstructor t is) = toJSON ("IsConstructor", constructorTypeToJSON t, map identToJSON is)
metaToJSON IsNewtype = toJSON ["IsNewtype"]
metaToJSON IsTypeClassConstructor = toJSON ["IsTypeClassConstructor"]
metaToJSON IsForeign = toJSON ["IsForeign"]

constructorTypeToJSON :: ConstructorType -> Value
constructorTypeToJSON ProductType = toJSON ["ProductType"]
constructorTypeToJSON SumType = toJSON ["SumType"]

{-
sourceSpanToJSON :: SourceSpan -> Value
sourceSpanToJSON (SourceSpan name start end) =
  object [ pack "spanName" .= name
         , pack "spanStart" .= sourcePosToJSON start
         , pack "spanEnd" .= sourcePosToJSON end
         ]

sourcePosToJSON :: SourcePos -> Value
sourcePosToJSON (SourcePos line col) =
  object [ pack "sourcePosLine" .= line
         , pack "sourcePosColumn" .= col
         ]
-}

identToJSON :: Ident -> Value
identToJSON (Ident s) = toJSON ("Ident", s)
identToJSON (GenIdent s i) = toJSON ("GenIdent", s, i)

qualifiedToJSON :: (a -> Value) -> Qualified a -> Value
qualifiedToJSON t (Qualified m i) =
  toJSON (fmap moduleNameToJSON m, t i)

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON (ModuleName ss) = toJSON (map properNameToJSON ss)

properNameToJSON :: ProperName a -> Value
properNameToJSON (ProperName n) = toJSON n

commentToJSON :: Comment -> Value
commentToJSON (LineComment s) = toJSON ("LineComment", s)
commentToJSON (BlockComment s) = toJSON ("BlockComment", s)

moduleToJSON :: (a -> Value) -> Module a -> Value
moduleToJSON t m = toJSON ( "Module"
                          , object [ pack "moduleComments" .= map commentToJSON (moduleComments m)
                                   , pack "moduleName" .= moduleNameToJSON (moduleName m)
                                   , pack "moduleImports" .= map (moduleImportToJSON t) (moduleImports m)
                                   , pack "moduleExports" .= map identToJSON (moduleExports m)
                                   , pack "moduleForeign" .= map foreignDeclToJSON (moduleForeign m)
                                   , pack "moduleDecls" .= map (bindToJSON t) (moduleDecls m)
                                   ]
                          )

moduleImportToJSON :: (a -> Value) -> (a, ModuleName) -> Value
moduleImportToJSON t (a, n) = toJSON (t a, moduleNameToJSON n)

foreignDeclToJSON :: ForeignDecl -> Value
foreignDeclToJSON (i, t) = toJSON (identToJSON i, typeToJSON t)

typeToJSON :: Type -> Value
typeToJSON = const Null
{-
typeToJSON (TUnknown n) = toJSON ("TUnknown", n)
typeToJSON (TypeVar v) = toJSON ("TypeVar", v)
typeToJSON (TypeLevelString s) = toJSON ("TypeLevelString", s)
typeToJSON (TypeWildcard s) = toJSON ("TypeWildcard", sourceSpanToJSON s)
typeToJSON (TypeConstructor q) = toJSON ("TypeConstructor", qualifiedToJSON properNameToJSON q)
typeToJSON (TypeOp n) = toJSON ("TypeOp", qualifiedToJSON opNameToJSON n)
typeToJSON (TypeApp f x) = toJSON ("TypeApp", typeToJSON f, typeToJSON x)
typeToJSON (ForAll s t ss) = toJSON ("ForAll", s, typeToJSON t, skolemScopeToJSON <$> ss)
typeToJSON (ConstrainedType cs t) = toJSON ("ConstrainedType", map constraintToJSON cs, typeToJSON t)
typeToJSON (Skolem s i sc ss) = toJSON ("Skolem", s, i, skolemScopeToJSON sc, sourceSpanToJSON <$> ss)
typeToJSON REmpty = toJSON ["REmpty"]
typeToJSON (RCons s t tl) = toJSON ("RCons", s, typeToJSON t, typeToJSON tl)
typeToJSON (KindedType t k) = toJSON ("KindedType", typeToJSON t, kindToJSON k)
typeToJSON PrettyPrintFunction{} = internalError "CoreFn.ToJSON: PrettyPrintFunction was not erased"
typeToJSON PrettyPrintObject{} = internalError "CoreFn.ToJSON: PrettyPrintObject was not erased"
typeToJSON PrettyPrintForAll{} = internalError "CoreFn.ToJSON: PrettyPrintForAll was not erased"
typeToJSON BinaryNoParensType{} = internalError "CoreFn.ToJSON: BinaryNoParensType was not erased"
typeToJSON ParensInType{} = internalError "CoreFn.ToJSON: ParensInType was not erased"

kindToJSON :: Kind -> Value
kindToJSON (KUnknown n) = toJSON ("KUnknown", n)
kindToJSON Star = toJSON ["Star"]
kindToJSON Bang = toJSON ["Bang"]
kindToJSON (Row boat) = toJSON ("Row", kindToJSON boat)
kindToJSON (FunKind d c) = toJSON ("FunKind", kindToJSON d, kindToJSON c)
kindToJSON Symbol = toJSON ["Symbol"]

opNameToJSON :: OpName a -> Value
opNameToJSON (OpName s) = toJSON ("OpName", s)

constraintToJSON :: Constraint -> Value
constraintToJSON (Constraint cls args dat) =
  object [ pack "constraintClass" .= qualifiedToJSON properNameToJSON cls
         , pack "constraintArgs" .= map typeToJSON args
         , pack "constraintData" .= (constraintDataToJSON <$> dat)
         ]

constraintDataToJSON :: ConstraintData -> Value
constraintDataToJSON (PartialConstraintData cs b) =
  toJSON ("PartialConstraintData", cs, b)

skolemScopeToJSON :: SkolemScope -> Value
skolemScopeToJSON (SkolemScope n) = toJSON ("SkolemScope", n)
-}

bindToJSON :: (a -> Value) -> Bind a -> Value
bindToJSON t (NonRec a n e) = toJSON ("NonRec", t a, identToJSON n, exprToJSON t e)
bindToJSON t (Rec bs) = toJSON ("Rec", map go bs)
  where go ((a, n), e) = toJSON ((t a, identToJSON n), exprToJSON t e)

exprToJSON :: (a -> Value) -> Expr a -> Value
exprToJSON t (Literal a l) = toJSON ("Literal", t a, literalToJSON (exprToJSON t) l)
exprToJSON t (Constructor a d c is) =
  toJSON ("Constructor", t a, properNameToJSON d, properNameToJSON c, map identToJSON is)
exprToJSON t (Accessor a f r) = toJSON ("Accessor", t a, f, exprToJSON t r)
exprToJSON t (ObjectUpdate a r fs) =
  toJSON ("ObjectUpdate", t a, exprToJSON t r, map (fmap (exprToJSON t)) fs)
exprToJSON t (Abs a p b) = toJSON ("Abs", t a, identToJSON p, exprToJSON t b)
exprToJSON t (App a f x) = toJSON ("App", t a, exprToJSON t f, exprToJSON t x)
exprToJSON t (Var a i) = toJSON ("Var", t a, qualifiedToJSON identToJSON i)
exprToJSON t (Case a ss cs) =
  toJSON ("Case", t a, map (exprToJSON t) ss, map (caseAlternativeToJSON t) cs)
exprToJSON t (Let a bs e) = toJSON ("Let", t a, map (bindToJSON t) bs, exprToJSON t e)

caseAlternativeToJSON :: (a -> Value) -> CaseAlternative a -> Value
caseAlternativeToJSON t (CaseAlternative bs r') =
  object [ pack "caseAlternativeBinders" .= map (binderToJSON t) bs
         , pack "caseAlternativeResult" .=
             case r' of
               Left rs -> toJSON ( "Left"
                                 , map (\(g, e) -> (exprToJSON t g, exprToJSON t e))
                                       rs
                                 )
               Right r -> toJSON ("Right", exprToJSON t r)
         ]

binderToJSON :: (a -> Value) -> Binder a -> Value
binderToJSON t (NullBinder a) = toJSON ("NullBinder", t a)
binderToJSON t (LiteralBinder a l) =
  toJSON ("LiteralBinder", t a, literalToJSON (binderToJSON t) l)
binderToJSON t (VarBinder a v) = toJSON ("VarBinder", t a, identToJSON v)
binderToJSON t (ConstructorBinder a d c bs) =
  toJSON ( "ConstructorBinder"
         , t a
         , qualifiedToJSON properNameToJSON d
         , qualifiedToJSON properNameToJSON c
         , map (binderToJSON t) bs
         )
binderToJSON t (NamedBinder a n b) =
  toJSON ("NamedBinder", t a, identToJSON n, binderToJSON t b)
