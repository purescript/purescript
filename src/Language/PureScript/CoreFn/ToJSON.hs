{-
Copyright (c) 2016, rightfold
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
may be used to endorse or promote products derived from this software without
specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Language.PureScript.CoreFn.ToJSON where

import Prelude.Compat

import Data.Aeson
import Data.Text (pack)

import Language.PureScript.AST.Literals
import Language.PureScript.Comments
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.Types

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
typeToJSON _ = Null

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
