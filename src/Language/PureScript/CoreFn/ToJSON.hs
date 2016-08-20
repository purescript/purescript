module Language.PureScript.CoreFn.ToJSON where

import Prelude.Compat

import Data.Aeson
import Data.Text (pack)

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.Comments
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Kinds

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON _ (NumericLiteral (Left n)) = toJSON ("NumericLiteral", ("Left", n))
literalToJSON _ (NumericLiteral (Right n)) = toJSON ("NumericLiteral", ("Right", n))
literalToJSON _ (StringLiteral s) = toJSON ("StringLiteral", s)
literalToJSON _ (CharLiteral c) = toJSON ("CharLiteral", c)
literalToJSON _ (BooleanLiteral b) = toJSON ("BooleanLiteral", b)
literalToJSON t (ArrayLiteral xs) = toJSON ("ArrayLiteral", map t xs)
literalToJSON t (ObjectLiteral xs) = toJSON ("ObjectLiteral", map (fmap t) xs)

metaToJSON :: Meta -> Value
metaToJSON (IsConstructor t is) = toJSON ("IsConstructor", constructorTypeToJSON t, map identToJSON is)
metaToJSON IsNewtype = toJSON ["IsNewtype"]
metaToJSON IsTypeClassConstructor = toJSON ["IsTypeClassConstructor"]
metaToJSON IsForeign = toJSON ["IsForeign"]

annToJSON :: Ann -> Value
annToJSON (ss, cs, t, m) = toJSON ( sourceSpanToJSON <$> ss
                                  , map commentToJSON cs
                                  , typeToJSON <$> t
                                  , metaToJSON <$> m
                                  )

constructorTypeToJSON :: ConstructorType -> Value
constructorTypeToJSON ProductType = toJSON ["ProductType"]
constructorTypeToJSON SumType = toJSON ["SumType"]

identToJSON :: Ident -> Value
identToJSON (Ident s) = toJSON ("Ident", s)
identToJSON (GenIdent s i) = toJSON ("GenIdent", s, i)

qualifiedToJSON :: (a -> Value) -> Qualified a -> Value
qualifiedToJSON t (Qualified m i) =
  toJSON ("Qualified", fmap moduleNameToJSON m, t i)

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON (ModuleName ss) = toJSON ("ModuleName", map properNameToJSON ss)

properNameToJSON :: ProperName a -> Value
properNameToJSON (ProperName n) = toJSON ("ProperName", n)

opNameToJSON :: OpName a -> Value
opNameToJSON (OpName s) = toJSON ("OpName", s)

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

kindToJSON :: Kind -> Value
kindToJSON (KUnknown n) = toJSON ("KUnknown", n)
kindToJSON Star = toJSON ["Star"]
kindToJSON Bang = toJSON ["Bang"]
kindToJSON (Row boat) = toJSON ("Row", kindToJSON boat)
kindToJSON (FunKind d c) = toJSON ("FunKind", kindToJSON d, kindToJSON c)
kindToJSON Symbol = toJSON ["Symbol"]

typeToJSON :: Type -> Value
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
typeToJSON PrettyPrintFunction{} = error "this shouldn't be here"
typeToJSON PrettyPrintObject{} = error "this shouldn't be here"
typeToJSON PrettyPrintForAll{} = error "this shouldn't be here"
typeToJSON BinaryNoParensType{} = error "this should have been removed"
typeToJSON ParensInType{} = error "this should have been removed"

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
