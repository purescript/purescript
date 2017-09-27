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
import Language.PureScript.CoreFn (Module, moduleImports, moduleExports, moduleForeign)
import Language.PureScript.CoreImp.AST
import Language.PureScript.Names
import Language.PureScript.PSString



identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON = toJSON . runModuleName

moduleToJSON :: Version -> Module a -> [AST] -> Value
moduleToJSON v m a = object [ T.pack "imports"   .= map (moduleNameToJSON . snd) (moduleImports m)
                            , T.pack "exports"   .= map identToJSON (moduleExports m)
                            , T.pack "foreign"   .= map (identToJSON . fst) (moduleForeign m)
                            , T.pack "builtWith" .= toJSON (showVersion v)
                            , T.pack "body"      .= map (astToJSON) a
                            ]

astToJSON :: AST -> Value
astToJSON (StringLiteral maybeSrcSpan psStr) =
    object [T.pack "sourceSpan"    .= toJSON maybeSrcSpan,
            T.pack "_StringLiteral" .= toJSON psStr ]
astToJSON (BooleanLiteral maybeSrcSpan bool) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_BooleanLiteral"     .= toJSON bool ]
astToJSON (NumericLiteral maybeSrcSpan (Left num)) =
    object [T.pack "sourceSpan"      .= toJSON maybeSrcSpan,
            T.pack "_NumericLiteral_Integer" .= toJSON num ]
astToJSON (NumericLiteral maybeSrcSpan (Right num)) =
    object [T.pack "sourceSpan"      .= toJSON maybeSrcSpan,
            T.pack "_NumericLiteral_Double" .= toJSON num ]
astToJSON (Var maybeSrcSpan text) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_Var"        .= toJSON text ]
astToJSON (Block maybeSrcSpan asts) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_Block"     .= map astToJSON asts ]
astToJSON (While maybeSrcSpan astCond astBody) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_While"     .= astToJSON astCond,
            T.pack "body"        .= astToJSON astBody ]
astToJSON (App maybeSrcSpan astFuncExpr astArgs) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_App"        .= astToJSON astFuncExpr,
            T.pack "args"        .= map astToJSON astArgs ]
astToJSON (Unary maybeSrcSpan unOp ast) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_Unary"    .= astToJSON ast,
            T.pack "op"         .= show unOp ]
astToJSON (Comment maybeSrcSpan comments ast) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_Comment"   .= map toJSON comments,
            T.pack "decl"       .= astToJSON ast ]
astToJSON (Function maybeSrcSpan maybeText texts ast) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_Function"  .= toJSON maybeText,
            T.pack "args"       .= toJSON texts,
            T.pack "body"        .= astToJSON ast ]
astToJSON (Binary maybeSrcSpan binOp astLeft astRight) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_Binary"    .= astToJSON astLeft,
            T.pack "op"         .= show binOp,
            T.pack "rhs"        .= astToJSON astRight ]
astToJSON (ForIn maybeSrcSpan forname astInExpr astBody) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_ForIn"     .= forname,
            T.pack "in"        .= astToJSON astInExpr,
            T.pack "body"        .= astToJSON astBody ]
astToJSON (For maybeSrcSpan forname astCond astStep astBody) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_ForIn"     .= forname,
            T.pack "cond"        .= astToJSON astCond,
            T.pack "step"        .= astToJSON astStep,
            T.pack "body"        .= astToJSON astBody ]
astToJSON (IfElse maybeSrcSpan astIf astThen astElse) =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "_IfElse"    .= astToJSON astIf,
            T.pack "then"       .= astToJSON astThen,
            T.pack "else"       .= subAstToJSONMaybe astElse ]
astToJSON (VariableIntroduction maybeSrcSpan text maybeAST) =
    object [T.pack "sourceSpan"           .= toJSON maybeSrcSpan,
            T.pack "_VariableIntroduction" .= toJSON text,
            T.pack "rhs"                  .= subAstToJSONMaybe maybeAST ]
astToJSON (ObjectLiteral maybeSrcSpan psStrASTs) =
    object [T.pack "sourceSpan"    .= toJSON maybeSrcSpan,
            T.pack "_ObjectLiteral" .= map psStrASTToJson psStrASTs ]
astToJSON (ReturnNoResult maybeSrcSpan) =
    object [T.pack "sourceSpan"      .= toJSON maybeSrcSpan,
            T.pack "_ReturnNoResult" .= Null ]
astToJSON (Return maybeSrcSpan ast) =
    subASTSingleToJSON "_Return" maybeSrcSpan ast
astToJSON (Throw maybeSrcSpan ast) =
    subASTSingleToJSON "_Throw" maybeSrcSpan ast
astToJSON (ArrayLiteral maybeSrcSpan asts) =
    subASTsListToJSON "_ArrayLiteral" maybeSrcSpan asts
astToJSON (Assignment maybeSrcSpan astLeft astRight) =
    subASTsLeftRightToJSON "_Assignment" maybeSrcSpan astLeft astRight
astToJSON (Indexer maybeSrcSpan astInner astOuter) =
    subASTsLeftRightToJSON "_Indexer" maybeSrcSpan astOuter astInner
astToJSON (InstanceOf maybeSrcSpan astLeft astRight) =
    subASTsLeftRightToJSON "_InstanceOf" maybeSrcSpan astLeft astRight

psStrASTToJson :: (PSString, AST) -> Value
psStrASTToJson (psStr, ast) =
    object [T.pack (decodeStringWithReplacement psStr) .= astToJSON ast]

subAstToJSONMaybe :: Maybe AST -> Value
subAstToJSONMaybe (Just ast) =
    astToJSON ast
subAstToJSONMaybe Nothing =
    Null

subASTSingleToJSON :: String -> (Maybe SourceSpan) -> AST -> Value
subASTSingleToJSON name maybeSrcSpan ast =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack name         .= astToJSON ast
            ]

subASTsListToJSON :: String -> (Maybe SourceSpan) -> [AST] -> Value
subASTsListToJSON name maybeSrcSpan asts =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack name         .= map astToJSON asts
            ]

subASTsLeftRightToJSON :: String -> (Maybe SourceSpan) -> AST -> AST -> Value
subASTsLeftRightToJSON name maybeSrcSpan astLeft astRight =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack name         .= astToJSON astLeft,
            T.pack "rhs"        .= astToJSON astRight
            ]
