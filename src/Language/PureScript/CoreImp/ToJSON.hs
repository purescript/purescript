{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core imperative representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreImp.ToJSON
  ( Language.PureScript.CoreImp.ToJSON.moduleToJSON
  ) where


import Prelude.Compat

import Data.Aeson
import qualified Data.Text as T
import Data.Version (Version)

import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn (Module)
import Language.PureScript.CoreFn.ToJSON as CoreFnToJSON
import Language.PureScript.CoreImp.AST
import Language.PureScript.PSString




moduleToJSON :: (Show a) => Version -> Module a -> [AST] -> Value
moduleToJSON v m a = object [ T.pack "body"    .= map (astToJSON) a
                            , T.pack "corefn"  .= CoreFnToJSON.moduleToJSON v m -- might be good to "keep a snapshot of the original FP logic of the module" depending on coreimp-processing use-case (others can ignore it)
                            , T.pack "modraw"  .= toJSON (show m) -- only way to capture type/ctors info without rewriting corefn, good enough for those needing it badly enough, others can ignore it
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
astToJSON (For maybeSrcSpan forname astCond astStep astBody) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= "For",
            T.pack "For"        .= forname,
            T.pack "for1"       .= astToJSON astCond,
            T.pack "for2"       .= astToJSON astStep,
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
