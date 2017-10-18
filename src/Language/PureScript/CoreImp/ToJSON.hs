{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core imperative representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreImp.ToJSON
  ( moduleToJSON
  ) where


import Prelude.Compat

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import Data.Version (Version, showVersion)

import Language.PureScript.AST (SourceSpan)
import qualified Language.PureScript.AST.Literals as ASTL
import Language.PureScript.CoreFn (Module, moduleComments, moduleDecls, moduleExports, moduleImports, moduleForeign, moduleName, modulePath)
import Language.PureScript.CoreFn.Binders
import qualified Language.PureScript.CoreFn.Expr as CoreFnExpr
import Language.PureScript.CoreImp.AST
import Language.PureScript.Names
import Language.PureScript.PSString


moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON = toJSON . runModuleName

literalToJSON :: (a -> Value) -> ASTL.Literal a -> Value
literalToJSON _ (ASTL.NumericLiteral (Left n)) = toJSON ("IntLiteral", n)
literalToJSON _ (ASTL.NumericLiteral (Right n)) = toJSON ("NumberLiteral", n)
literalToJSON _ (ASTL.StringLiteral s) = toJSON ("StringLiteral", s)
literalToJSON _ (ASTL.CharLiteral c) = toJSON ("CharLiteral", c)
literalToJSON _ (ASTL.BooleanLiteral b) = toJSON ("BooleanLiteral", b)
literalToJSON t (ASTL.ArrayLiteral xs) = toJSON ("ArrayLiteral", map t xs)
literalToJSON t (ASTL.ObjectLiteral xs) = toJSON ("ObjectLiteral", recordToJSON t xs)

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

qualifiedToJSON :: (a -> T.Text) -> Qualified a -> Value
qualifiedToJSON f = toJSON . showQualified f

moduleToJSON :: (ToJSON a) => Version -> Module a -> [AST] -> Value
moduleToJSON v m ast = object
  [ T.pack "moduleName" .= moduleNameToJSON (moduleName m)
  , T.pack "modulePath" .= toJSON (modulePath m)
  , T.pack "imports"   .= map (moduleNameToJSON . snd) (moduleImports m)
  , T.pack "exports"   .= map identToJSON (moduleExports m)
  , T.pack "foreign"   .= map toJSON (moduleForeign m)
  , T.pack "builtWith" .= toJSON (showVersion v)
  , T.pack "comments"   .= map toJSON (moduleComments m)
  , T.pack "decls"     .= map bindToJSON (moduleDecls m) -- unlike the `decls` in --dump-corefn, here we only dump annotations (types & meta)
  , T.pack "body"      .= map (astToJSON) ast
  ]

bindToJSON :: (ToJSON a) => CoreFnExpr.Bind a -> Value
bindToJSON (CoreFnExpr.NonRec _ n e) = object [ runIdent n .= exprToJSON e ]
bindToJSON (CoreFnExpr.Rec bs) = object $ map (\((_, n), e) -> runIdent n .= exprToJSON e) bs

recordToJSON :: (a -> Value) -> [(PSString, a)] -> Value
recordToJSON f rec = fromMaybe (asArrayOfPairs rec) (asObject rec)
  where
  asObject = fmap object . traverse (uncurry maybePair)
  maybePair label a = fmap (\l -> l .= f a) (decodeString label)

  asArrayOfPairs = toJSON . map (\(label, a) -> (toJSON label, f a))

exprToJSON :: (ToJSON a) => CoreFnExpr.Expr a -> Value
exprToJSON (CoreFnExpr.Var ann i)              = toJSON ( "Var"
                                               , qualifiedToJSON runIdent i
                                               , ann
                                               )
exprToJSON (CoreFnExpr.Literal ann l)          = toJSON ( "Literal"
                                               , literalToJSON (exprToJSON) l
                                               , ann
                                               )
exprToJSON (CoreFnExpr.Constructor ann d c is) = toJSON ( "Constructor"
                                               , properNameToJSON d
                                               , properNameToJSON c
                                               , map identToJSON is
                                               , ann
                                               )
exprToJSON (CoreFnExpr.Accessor ann f r)       = toJSON ( "Accessor"
                                               , f
                                               , exprToJSON r
                                               , ann
                                               )
exprToJSON (CoreFnExpr.ObjectUpdate ann r fs)  = toJSON ( "ObjectUpdate"
                                               , exprToJSON r
                                               , recordToJSON exprToJSON fs
                                               , ann
                                               )
exprToJSON (CoreFnExpr.Abs ann p b)            = toJSON ( "Abs"
                                               , identToJSON p
                                               , exprToJSON b
                                               , ann
                                               )
exprToJSON (CoreFnExpr.App ann f x)            = toJSON ( "App"
                                               , exprToJSON f
                                               , exprToJSON x
                                               , ann
                                               )
exprToJSON (CoreFnExpr.Case ann ss cs)         = toJSON ( "Case"
                                               , map exprToJSON ss
                                               , map caseAlternativeToJSON cs
                                               , ann
                                               )
exprToJSON (CoreFnExpr.Let ann bs e)           = toJSON ( "Let"
                                               , map bindToJSON bs
                                               , exprToJSON e
                                               , ann
                                               )

caseAlternativeToJSON :: (ToJSON a) => CoreFnExpr.CaseAlternative a -> Value
caseAlternativeToJSON (CoreFnExpr.CaseAlternative bs r') =
  toJSON [ toJSON (map binderToJSON bs)
         , case r' of
             Left rs -> toJSON $ map (\(g, e) -> (exprToJSON g, exprToJSON e)) rs
             Right r -> exprToJSON r
         ]

binderToJSON :: (ToJSON a) => Binder a -> Value
binderToJSON (VarBinder ann v)              = toJSON ( "VarBinder"
                                                     , identToJSON v
                                                     , ann
                                                     )
binderToJSON (NullBinder ann)               = toJSON ( "NullBinder"
                                                     , ann
                                                     )
binderToJSON (LiteralBinder ann l)          = toJSON ( "LiteralBinder"
                                                     , literalToJSON binderToJSON l
                                                     , ann
                                                     )
binderToJSON (ConstructorBinder ann d c bs) = toJSON ( "ConstructorBinder"
                                                     , qualifiedToJSON runProperName d
                                                     , qualifiedToJSON runProperName c
                                                     , map binderToJSON bs
                                                     , ann
                                                     )
binderToJSON (NamedBinder ann n b)          = toJSON ( "NamedBinder"
                                                     , identToJSON n
                                                     , binderToJSON b
                                                     , ann
                                                     )





astToJSON :: AST -> Value
astToJSON (StringLiteral maybeSrcSpan psStr) =
    object [T.pack "sourceSpan"    .= toJSON maybeSrcSpan,
            T.pack "tag"           .= "StringLiteral",
            T.pack "StringLiteral" .= toJSON psStr ]
astToJSON (BooleanLiteral maybeSrcSpan bool) =
    object [T.pack "sourceSpan"     .= toJSON maybeSrcSpan,
            T.pack "tag"            .= "BooleanLiteral",
            T.pack "BooleanLiteral" .= toJSON bool ]
astToJSON (NumericLiteral maybeSrcSpan (Left num)) =
    object [T.pack "sourceSpan"             .= toJSON maybeSrcSpan,
            T.pack "tag"                    .= "NumericLiteral_Integer",
            T.pack "NumericLiteral_Integer" .= toJSON num ]
astToJSON (NumericLiteral maybeSrcSpan (Right num)) =
    object [T.pack "sourceSpan"            .= toJSON maybeSrcSpan,
            T.pack "tag"                   .= "NumericLiteral_Double",
            T.pack "NumericLiteral_Double" .= toJSON num ]
astToJSON (Var maybeSrcSpan text) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "Var",
            T.pack "Var"        .= toJSON text ]
astToJSON (Block maybeSrcSpan asts) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "Block",
            T.pack "Block"      .= map astToJSON asts ]
astToJSON (While maybeSrcSpan astCond astBody) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "While",
            T.pack "While"      .= astToJSON astCond,
            T.pack "body"       .= astToJSON astBody ]
astToJSON (App maybeSrcSpan astFuncExpr astArgs) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "App",
            T.pack "App"        .= astToJSON astFuncExpr,
            T.pack "args"       .= map astToJSON astArgs ]
astToJSON (Unary maybeSrcSpan unOp ast) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "Unary",
            T.pack "Unary"      .= astToJSON ast,
            T.pack "op"         .= show unOp ]
astToJSON (Comment maybeSrcSpan comments ast) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "Comment",
            T.pack "Comment"    .= map toJSON comments,
            T.pack "decl"       .= astToJSON ast ]
astToJSON (Function maybeSrcSpan maybeText texts ast) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "Function",
            T.pack "Function"   .= toJSON maybeText,
            T.pack "params"     .= toJSON texts,
            T.pack "body"       .= astToJSON ast ]
astToJSON (Binary maybeSrcSpan binOp astLeft astRight) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "Binary",
            T.pack "Binary"     .= astToJSON astLeft,
            T.pack "op"         .= show binOp,
            T.pack "rhs"        .= astToJSON astRight ]
astToJSON (ForIn maybeSrcSpan forname astInExpr astBody) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "ForIn",
            T.pack "ForIn"      .= forname,
            T.pack "for1"       .= astToJSON astInExpr,
            T.pack "body"       .= astToJSON astBody ]
astToJSON (For maybeSrcSpan forname astInitial astCmpLT astBody) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "For",
            T.pack "For"        .= forname,
            T.pack "for1"       .= astToJSON astInitial,
            T.pack "for2"       .= astToJSON astCmpLT,
            T.pack "body"       .= astToJSON astBody ]
astToJSON (IfElse maybeSrcSpan astIf astThen astElse) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "IfElse",
            T.pack "IfElse"     .= astToJSON astIf,
            T.pack "then"       .= astToJSON astThen,
            T.pack "else"       .= subAstToJSON astElse ]
astToJSON (VariableIntroduction maybeSrcSpan text maybeAST) =
    object [T.pack "sourceSpan"           .= toJSON maybeSrcSpan,
            T.pack "tag"                  .= "VariableIntroduction",
            T.pack "VariableIntroduction" .= toJSON text,
            T.pack "rhs"                  .= subAstToJSON maybeAST ]
astToJSON (ObjectLiteral maybeSrcSpan psStrASTs) =
    object [T.pack "sourceSpan"    .= toJSON maybeSrcSpan,
            T.pack "tag"           .= "ObjectLiteral",
            T.pack "ObjectLiteral" .= map psStrASTToJson psStrASTs ]
astToJSON (ReturnNoResult maybeSrcSpan) =
    object [T.pack "sourceSpan"     .= toJSON maybeSrcSpan,
            T.pack "tag"            .= "ReturnNoResult" ]
astToJSON (Return maybeSrcSpan ast) =
    subASTSingleToJSON "Return" maybeSrcSpan ast
astToJSON (Throw maybeSrcSpan ast) =
    subASTSingleToJSON "Throw" maybeSrcSpan ast
astToJSON (ArrayLiteral maybeSrcSpan asts) =
    subASTsListToJSON "ArrayLiteral" maybeSrcSpan asts
astToJSON (Assignment maybeSrcSpan astLeft astRight) =
    subASTsLeftRightToJSON "Assignment" maybeSrcSpan astLeft astRight
astToJSON (Indexer maybeSrcSpan astInner astOuter) =
    subASTsLeftRightToJSON "Indexer" maybeSrcSpan astOuter astInner
astToJSON (InstanceOf maybeSrcSpan astLeft astRight) =
    subASTsLeftRightToJSON "InstanceOf" maybeSrcSpan astLeft astRight

psStrASTToJson :: (PSString, AST) -> Value
psStrASTToJson (psStr, ast) =
    object [T.pack (decodeStringWithReplacement psStr) .= astToJSON ast]

subAstToJSON :: Maybe AST -> Value
subAstToJSON (Just ast) =
    astToJSON ast
subAstToJSON Nothing =
    Null

subASTSingleToJSON :: String -> Maybe SourceSpan -> AST -> Value
subASTSingleToJSON tag maybeSrcSpan ast =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= tag,
            T.pack tag          .= astToJSON ast
            ]

subASTsListToJSON :: String -> Maybe SourceSpan -> [AST] -> Value
subASTsListToJSON tag maybeSrcSpan asts =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= tag,
            T.pack tag          .= map astToJSON asts
            ]

subASTsLeftRightToJSON :: String -> Maybe SourceSpan -> AST -> AST -> Value
subASTsLeftRightToJSON tag maybeSrcSpan astLeft astRight =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= tag,
            T.pack tag          .= astToJSON astLeft,
            T.pack "rhs"        .= astToJSON astRight
            ]
