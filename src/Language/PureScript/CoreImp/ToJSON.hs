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
import qualified Data.Text as T
import Data.Version (Version, showVersion)

import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn (Ann, Module, moduleComments, moduleDecls, moduleExports, moduleImports, moduleForeign, moduleName, modulePath)
import qualified Language.PureScript.CoreFn.Expr as CFnExpr
import Language.PureScript.CoreImp.AST
import Language.PureScript.Names
import Language.PureScript.PSString


moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON = toJSON . runModuleName

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

moduleToJSON :: Version -> Module Ann -> [AST] -> Value
moduleToJSON v m ast = object
  [ T.pack "moduleName" .= moduleNameToJSON (moduleName m)
  , T.pack "modulePath" .= toJSON (modulePath m)
  , T.pack "imports"    .= map (moduleNameToJSON . snd) (moduleImports m)
  , T.pack "exports"    .= map identToJSON (moduleExports m)
  , T.pack "foreign"    .= map identToJSON (moduleForeign m)
  , T.pack "builtWith"  .= toJSON (showVersion v)
  , T.pack "comments"   .= map toJSON (moduleComments m)
  , T.pack "declAnns"   .= map bindToJSON (moduleDecls m) -- all top-level decls (not just the exporteds as in externs.json) --- anns/meta/types
  , T.pack "body"       .= map (astToJSON) ast
  ]

bindToJSON :: (ToJSON a) => CFnExpr.Bind a -> Value
bindToJSON (CFnExpr.NonRec _ n e) = object [ runIdent n .= exprToJSON e ]
bindToJSON (CFnExpr.Rec bs) = object $ map (\((_, n), e) -> runIdent n .= exprToJSON e) bs

exprToJSON :: (ToJSON a) => CFnExpr.Expr a -> Value
exprToJSON (CFnExpr.Var ann _)              = object [ T.pack "type"             .= "Var"
                                                     , T.pack "annotation"       .= toJSON ann]
exprToJSON (CFnExpr.Literal ann _)          = object [ T.pack "type"             .= "Literal"
                                                     , T.pack "annotation"       .= toJSON ann]
exprToJSON (CFnExpr.Accessor ann _ _)       = object [ T.pack "type"             .= "Accessor"
                                                     , T.pack "annotation"       .= toJSON ann]
exprToJSON (CFnExpr.ObjectUpdate ann _ _)   = object [ T.pack "type"             .= "ObjectUpdate"
                                                     , T.pack "annotation"       .= toJSON ann]
exprToJSON (CFnExpr.Abs ann _ _)            = object [ T.pack "type"             .= "Abs"
                                                     , T.pack "annotation"       .= toJSON ann]
exprToJSON (CFnExpr.App ann _ _)            = object [ T.pack "type"             .= "App"
                                                     , T.pack "annotation"       .= toJSON ann]
exprToJSON (CFnExpr.Case ann _ _)           = object [ T.pack "type"             .= "Case"
                                                     , T.pack "annotation"       .= toJSON ann]
exprToJSON (CFnExpr.Let ann _ _)            = object [ T.pack "type"             .= "Let"
                                                     , T.pack "annotation"       .= toJSON ann]
exprToJSON (CFnExpr.Constructor ann d c is) = object [ T.pack "type"             .= "Constructor"
                                                     , T.pack "annotation"       .= toJSON ann
                                                     , T.pack "typeName"         .= properNameToJSON d
                                                     , T.pack "constructorName"  .= properNameToJSON c
                                                     , T.pack "fieldNames"       .= map identToJSON is
                                                     ]


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
