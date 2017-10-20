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
import Language.PureScript.Types
import Language.PureScript.TypeClassDictionaries

moduleToJSON :: Version -> Module Ann -> Environment -> [AST] -> Value
moduleToJSON v m env ast = object
  [ T.pack "moduleName" .= moduleNameToJSON mname
  , T.pack "modulePath" .= toJSON (modulePath m)
  , T.pack "builtWith"  .= toJSON (showVersion v)
  , T.pack "comments"   .= map toJSON (moduleComments m)
  , T.pack "imports"    .= map (moduleNameToJSON . snd) (moduleImports m)
  , T.pack "exports"    .= map identToJSON (moduleExports m)
  , T.pack "foreign"    .= map identToJSON (moduleForeign m)
  , T.pack "declAnns"   .= map bindToJSON (moduleDecls m) -- all top-level decls (not just the exporteds as in externs.json) --- anns/meta/types only
  , T.pack "declEnv"    .= ownEnvToJSON mname env
  , T.pack "body"       .= map astToJSON ast
  ] where mname = moduleName m


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
ownEnvToJSON ourmodname env = object
  [ T.pack "names"                  .= namesToJSON (names env)
  , T.pack "types"                  .= typesToJSON (types env)
  , T.pack "dataConstructors"       .= ctorsToJSON (dataConstructors env)
  , T.pack "typeSynonyms"           .= typeSynsToJSON (typeSynonyms env)
  , T.pack "typeClasses"            .= typeClassesToJSON (typeClasses env)
  , T.pack "typeClassDictionaries"  .= typeClassDictsToJSON (typeClassDictionaries env)
  -- , T.pack "kinds"                  .= toJSON (kinds env) -- doesn't seem useful for dump-coreimp purposes yet
  ]
  where
    isown     (Just modname)                = (modname == ourmodname)
    isown     _                             = True -- no-module-given means 'current', no?
    isOwn     (Qualified maybemodname _, _) = isown maybemodname
    toJSONObj pair2json                     = object . (map pair2json) . (filter isOwn) . M.toList

    namesToJSON = toJSONObj pair2json where
      pair2json (Qualified _ ident, (nametype, namekind, namevis)) =
        showIdent ident .= object [ T.pack "nType" .= toJSON nametype
                                  , T.pack "nKind" .= show namekind
                                  , T.pack "nVis"  .= show namevis]

    ctorsToJSON = toJSONObj pair2json where
      pair2json (Qualified _ pn, (decl, typename, typetag, idents)) =
        runProperName pn .= object [ T.pack "cDecl" .= toJSON decl -- data or newtype
                                   , T.pack "cType" .= runProperName typename
                                   , T.pack "cCtor" .= toJSON typetag -- TypeConstructor{} or ForAll{} or TypeApp{} etc etc
                                   , T.pack "cArgs" .= map showIdent idents ] -- value0, value1 etc

    typeSynsToJSON = toJSONObj pair2json where
      pair2json (Qualified _ pn, (typeargs, typetag)) =
        runProperName pn .= object [ T.pack "tsType" .= toJSON typetag
                                   , T.pack "tsArgs" .= object (map argp2jsonp typeargs) ]
      argp2jsonp (argname, argkind) =
        argname .= toJSON argkind

    typesToJSON = toJSONObj pair2json where
      pair2json (Qualified _ pn, (kind, tkind)) =
        runProperName pn .= object [ T.pack "tKind" .= toJSON kind -- FunKind{} or NamedKind{} or etc
                                   , T.pack "tDecl" .= typekind2json tkind ]
      typekind2json (DataType args ctors) =
        object [ T.pack "DataType" .= object
               [ T.pack "args" .= object (map (\(t,k) -> t.=k) args)
               , T.pack "ctors" .= object (map (\(n,t) -> runProperName n .= map toJSON t) ctors) ]]
      typekind2json other =
        object [ T.pack (show other) .= True ] -- this way tDecl has always an object instead of polymorphic -- decodes way better usually

    typeClassesToJSON = toJSONObj pair2json where
      pair2json (Qualified _ pn, tcdata) =
        runProperName pn .= object [ T.pack "tcCoveringSets"   .= toJSON (typeClassCoveringSets tcdata)
                                   , T.pack "tcDeterminedArgs" .= toJSON (typeClassDeterminedArguments tcdata)
                                   , T.pack "tcArgs"           .= object (map tcargp2jsonp (typeClassArguments tcdata))
                                   , T.pack "tcMembers"        .= object (map tcmemberp2jsonp (typeClassMembers tcdata))
                                   , T.pack "tcSuperclasses"   .= map tcConstraintToJSON (typeClassSuperclasses tcdata)
                                   , T.pack "tcDependencies"   .= map toJSON (typeClassDependencies tcdata) ]
      tcmemberp2jsonp (tcmident, tcmtype) =
        showIdent tcmident .= toJSON tcmtype
      tcargp2jsonp (argname,argkind) =
        argname .= toJSON argkind

    typeClassDictsToJSON =
      toJSON . (map (object . (map pair2json) . M.toList . snd)) . (filter $ isown.fst) . M.toList
      where
        pair2json (qn, m) =
          qkey qn .= object (map iddict2jsonp (M.toList m))
        iddict2jsonp (Qualified _ ident, dict) =
          showIdent ident .= object [ T.pack "tcdChain"         .= map ikey (tcdChain dict)
                                    , T.pack "tcdIndex"         .= toJSON (tcdIndex dict)
                                    , T.pack "tcdValue"         .= ikey (tcdValue dict)
                                    , T.pack "tcdPath"          .= object (map tcpath2jsonp (tcdPath dict))
                                    , T.pack "tcdClassName"     .= qkey (tcdClassName dict)
                                    , T.pack "tcdInstanceTypes" .= toJSON (tcdInstanceTypes dict)
                                    , T.pack "tcdDependencies"  .= maybe [] (map tcConstraintToJSON) (tcdDependencies dict) ]
        tcpath2jsonp (qn, i) =
          qkey qn .= i

    tcConstraintToJSON c = -- almost on-par with existing ToJSON except for qkey (for more tractable decoding)
      object [ T.pack "constraintClass" .= qkey (constraintClass c)
             , T.pack "constraintData"  .= constraintData c
             , T.pack "constraintArgs"  .= constraintArgs c ]

    ikey (Qualified _ ident) =
      showIdent ident
    qkey (Qualified Nothing pn) =
      runProperName pn
    qkey (Qualified (Just (ModuleName pns)) pn) =
      T.intercalate (T.pack ".") ((map runProperName pns) ++ [runProperName pn])


-- EVERYTHING BELOW: following in spirit from CoreFn/ToJSON, with minor tweaks for CoreImp/ToJSON purposes


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
exprToJSON (CFnExpr.Var ann _)              = object $ annPlusType ann "Var"
exprToJSON (CFnExpr.Literal ann _)          = object $ annPlusType ann "Literal"
exprToJSON (CFnExpr.Abs ann _ _)            = object $ annPlusType ann "Abs"
exprToJSON (CFnExpr.App ann _ _)            = object $ annPlusType ann "App"
exprToJSON (CFnExpr.Let ann _ _)            = object $ annPlusType ann "Let"
exprToJSON (CFnExpr.Constructor ann d c is) = object $ annPlusType ann "Constructor" ++
                                                        [ T.pack "typeName"         .= properNameToJSON d
                                                        , T.pack "constructorName"  .= properNameToJSON c
                                                        , T.pack "fieldNames"       .= map identToJSON is
                                                        ]
-- these below don't enter in practice as we don't need to traverse into CoreFn's
-- top-level defs for the purposes of coreimp.json, but for the sake of completeness:
exprToJSON (CFnExpr.Accessor ann _ _)       = object $ annPlusType ann "Accessor"
exprToJSON (CFnExpr.ObjectUpdate ann _ _)   = object $ annPlusType ann "ObjectUpdate"
exprToJSON (CFnExpr.Case ann _ _)           = object $ annPlusType ann "Case"


annPlusType :: (KeyValue a) => Ann -> String -> [a]
annPlusType ann typedesc =
  [T.pack "type" .= typedesc, T.pack "annotation" .= annToJSON ann]
