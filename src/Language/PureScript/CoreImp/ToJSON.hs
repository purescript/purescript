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
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Version (Version, showVersion)

import Language.PureScript.AST (SourceSpan)
import Language.PureScript.CoreFn (Ann, Module, moduleComments, moduleDecls, moduleExports, moduleImports, moduleForeign, moduleName, modulePath)
import qualified Language.PureScript.CoreFn.Expr as CFnExpr
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreFn.ToJSON (sourceSpanToJSON, metaToJSON, moduleNameToJSON, identToJSON, properNameToJSON)
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.PSString
import Language.PureScript.TypeClassDictionaries

moduleToJSON :: Version -> Module Ann -> Environment -> [AST] -> Value
moduleToJSON v m env ast = object
  [ T.pack "moduleName" .= moduleNameToJSON mname
  , T.pack "modulePath" .= toJSON (modulePath m)
  , T.pack "imports"    .= map (moduleNameToJSON . snd) (moduleImports m)
  , T.pack "exports"    .= map identToJSON (moduleExports m)
  , T.pack "foreign"    .= map identToJSON (moduleForeign m)
  , T.pack "builtWith"  .= toJSON (showVersion v)
  , T.pack "comments"   .= map toJSON (moduleComments m)
  , T.pack "decls"      .= ownEnvToJSON mname env
  , T.pack "declAnns"   .= map bindToJSON (moduleDecls m) -- all top-level decls (not just the exporteds as in externs.json) --- anns/meta/types only
  , T.pack "body"       .= map (astToJSON) ast
  ] where
    mname = moduleName m


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
            T.pack "else"       .= astSubToJSON astElse ]
astToJSON (VariableIntroduction maybeSrcSpan text maybeAST) =
    object [T.pack "sourceSpan"           .= toJSON maybeSrcSpan,
            T.pack "tag"                  .= "VariableIntroduction",
            T.pack "VariableIntroduction" .= toJSON text,
            T.pack "rhs"                  .= astSubToJSON maybeAST ]
astToJSON (ObjectLiteral maybeSrcSpan psStrASTs) =
    object [T.pack "sourceSpan"    .= toJSON maybeSrcSpan,
            T.pack "tag"           .= "ObjectLiteral",
            T.pack "ObjectLiteral" .= map astPSStrToJSON psStrASTs ]
astToJSON (ReturnNoResult maybeSrcSpan) =
    object [T.pack "sourceSpan"     .= toJSON maybeSrcSpan,
            T.pack "tag"            .= "ReturnNoResult" ]
astToJSON (Return maybeSrcSpan ast) =
    astSubSingleToJSON "Return" maybeSrcSpan ast
astToJSON (Throw maybeSrcSpan ast) =
    astSubSingleToJSON "Throw" maybeSrcSpan ast
astToJSON (ArrayLiteral maybeSrcSpan asts) =
    astSubListToJSON "ArrayLiteral" maybeSrcSpan asts
astToJSON (Assignment maybeSrcSpan astLeft astRight) =
    astSubLRToJSON "Assignment" maybeSrcSpan astLeft astRight
astToJSON (Indexer maybeSrcSpan astInner astOuter) =
    astSubLRToJSON "Indexer" maybeSrcSpan astOuter astInner
astToJSON (InstanceOf maybeSrcSpan astLeft astRight) =
    astSubLRToJSON "InstanceOf" maybeSrcSpan astLeft astRight


astPSStrToJSON :: (PSString, AST) -> Value
astPSStrToJSON (psStr, ast) =
    object [T.pack (decodeStringWithReplacement psStr) .= astToJSON ast]


astSubToJSON :: Maybe AST -> Value
astSubToJSON (Just ast) =
    astToJSON ast
astSubToJSON Nothing =
    Null


astSubSingleToJSON :: String -> Maybe SourceSpan -> AST -> Value
astSubSingleToJSON tag maybeSrcSpan ast =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= tag,
            T.pack tag          .= astToJSON ast
            ]


astSubListToJSON :: String -> Maybe SourceSpan -> [AST] -> Value
astSubListToJSON tag maybeSrcSpan asts =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= tag,
            T.pack tag          .= map astToJSON asts
            ]


astSubLRToJSON :: String -> Maybe SourceSpan -> AST -> AST -> Value
astSubLRToJSON tag maybeSrcSpan astLeft astRight =
    object [T.pack "sourceSpan" .= toJSON maybeSrcSpan,
            T.pack "tag"        .= tag,
            T.pack tag          .= astToJSON astLeft,
            T.pack "rhs"        .= astToJSON astRight
            ]


ownEnvToJSON :: ModuleName -> Environment -> Value
ownEnvToJSON mname env = object
  [ T.pack "names" .= namesToJSON (names env)
  , T.pack "types" .= typesToJSON (types env)
  , T.pack "dataConstructors" .= ctorsToJSON (dataConstructors env)
  , T.pack "typeSynonyms" .= typeSynsToJSON (typeSynonyms env)
  , T.pack "typeClasses" .= typeClassesToJSON (typeClasses env)
  , T.pack "typeClassDictionaries" .= typeClassDictsToJSON (typeClassDictionaries env)
  -- , T.pack "kinds" .= toJSON (kinds env) -- skip this work as for now doesn't seem useful for coreimp consumers
  ]
  where
    isown (Just modname) = (modname == mname)
    isown _              = True
    isOwn (Qualified maybemodname _) = isown maybemodname
    toObj p2j = object . (map p2j) . (filter (isOwn.fst)) . M.toList

    typesToJSON = toObj p2j where
      p2j (Qualified _ pn, (kind,tkind)) =
        runProperName pn .= object [ T.pack "kind"      .= toJSON kind
                                   , T.pack "typeKind"  .= toJSON tkind ]
    typeSynsToJSON = toObj p2j where
      p2j (Qualified _ pn, (typeargs,typetag)) =
        let vp2jp (vn, vk) = vn .= toJSON vk
        in runProperName pn .= object [ T.pack "type" .= toJSON typetag
                                      , T.pack "vars" .= object (map vp2jp typeargs) ]
    namesToJSON = toObj p2j where
      p2j (Qualified _ ident, (nametype, namekind, namevis)) =
        showIdent ident .= object [ T.pack "nameType" .= toJSON nametype
                                  , T.pack "nameKind" .= show namekind
                                  , T.pack "nameVis" .= show namevis]
    ctorsToJSON = toObj p2j where
      p2j (Qualified _ pn, (ddecltype, dptypename, dtype, didents)) =
        runProperName pn .= object [ T.pack "dataDeclType" .= toJSON ddecltype
                                   , T.pack "dataTypeName" .= runProperName dptypename
                                   , T.pack "dataType" .= toJSON dtype
                                   , T.pack "dataIdents" .= map showIdent didents ]
    typeClassesToJSON = toObj p2j where
      p2j (Qualified _ pn, tcdata) =
        let tcm2jp (tcmi,tcmt) = showIdent tcmi .= toJSON tcmt
        in runProperName pn .= object [ T.pack "tcCoveringSets" .= toJSON (typeClassCoveringSets tcdata)
                                      , T.pack "tcDeterminedArguments" .= toJSON (typeClassDeterminedArguments tcdata)
                                      , T.pack "tcArguments" .= toJSON (typeClassArguments tcdata)
                                      , T.pack "tcMembers" .= object (map tcm2jp (typeClassMembers tcdata))
                                      , T.pack "tcSuperclasses" .= map toJSON (typeClassSuperclasses tcdata)
                                      , T.pack "tcDependencies" .= map toJSON (typeClassDependencies tcdata) ]
    typeClassDictsToJSON =
      toJSON . (map (object . (map p2j) . M.toList . snd)) . (filter (isown.fst)) . M.toList
      where
        p2j (qn, m) =
          qkey qn .= object (map id2jp (M.toList m))
        qkey (Qualified Nothing pn) =
          runProperName pn
        qkey (Qualified (Just (ModuleName pns)) pn) =
          T.intercalate (T.pack ".") ((map runProperName pns) ++ [runProperName pn])
        id2jp (Qualified _ ident, dict) =
          showIdent ident .= object [ T.pack "tcdChain" .= toJSON (tcdChain dict)
                                    , T.pack "tcdIndex" .= toJSON (tcdIndex dict)
                                    , T.pack "tcdValue" .= toJSON (tcdValue dict)
                                    , T.pack "tcdPath" .= toJSON (tcdPath dict)
                                    , T.pack "tcdClassName" .= toJSON (tcdClassName dict)
                                    , T.pack "tcdInstanceTypes" .= toJSON (tcdInstanceTypes dict)
                                    , T.pack "tcdDependencies" .= maybe Null toJSON (tcdDependencies dict) ]


-- EVERYTHING BELOW: FOLLOWING FROM CoreFn/ToJSON, with tweaks


annToJSON :: Ann -> Value
annToJSON (ss, comments, maybeType, maybeMeta) = object
  [ T.pack "sourceSpan"  .= sourceSpanToJSON ss
  , T.pack "comments"    .= toJSON comments
  , T.pack "type"        .= maybe Null toJSON maybeType
  , T.pack "meta"        .= maybe Null metaToJSON maybeMeta
  ]


bindToJSON :: CFnExpr.Bind Ann -> Value
bindToJSON (CFnExpr.NonRec ann n e)
  = object
    [ T.pack "bindType"   .= "NonRec"
    , T.pack "annotation" .= annToJSON ann
    , T.pack "identifier" .= identToJSON n
    , T.pack "expression" .= exprToJSON e
    ]
bindToJSON (CFnExpr.Rec bs)
  = object
    [ T.pack "bindType"   .= "Rec"
    , T.pack "binds"      .= map (\((ann, n), e)
                                  -> object
                                      [ T.pack "identifier"  .= identToJSON n
                                      , T.pack "annotation"   .= annToJSON ann
                                      , T.pack "expression"   .= exprToJSON e
                                      ]) bs
    ]


exprToJSON :: CFnExpr.Expr Ann -> Value
exprToJSON (CFnExpr.Var ann _)              = object [ T.pack "type"             .= "Var"
                                                     , T.pack "annotation"       .= annToJSON ann]
exprToJSON (CFnExpr.Literal ann _)          = object [ T.pack "type"             .= "Literal"
                                                     , T.pack "annotation"       .= annToJSON ann]
exprToJSON (CFnExpr.Accessor ann _ _)       = object [ T.pack "type"             .= "Accessor"
                                                     , T.pack "annotation"       .= annToJSON ann]
exprToJSON (CFnExpr.ObjectUpdate ann _ _)   = object [ T.pack "type"             .= "ObjectUpdate"
                                                     , T.pack "annotation"       .= annToJSON ann]
exprToJSON (CFnExpr.Abs ann _ _)            = object [ T.pack "type"             .= "Abs"
                                                     , T.pack "annotation"       .= annToJSON ann]
exprToJSON (CFnExpr.App ann _ _)            = object [ T.pack "type"             .= "App"
                                                     , T.pack "annotation"       .= annToJSON ann]
exprToJSON (CFnExpr.Case ann _ _)           = object [ T.pack "type"             .= "Case"
                                                     , T.pack "annotation"       .= annToJSON ann]
exprToJSON (CFnExpr.Let ann _ _)            = object [ T.pack "type"             .= "Let"
                                                     , T.pack "annotation"       .= annToJSON ann]
exprToJSON (CFnExpr.Constructor ann d c is) = object [ T.pack "type"             .= "Constructor"
                                                     , T.pack "annotation"       .= annToJSON ann
                                                     , T.pack "typeName"         .= properNameToJSON d
                                                     , T.pack "constructorName"  .= properNameToJSON c
                                                     , T.pack "fieldNames"       .= map identToJSON is
                                                     ]
