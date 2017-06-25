-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Bundle.BundleOpt
-- Copyright   :  (c) Jürgen Nicklisch-Franken 2016
-- License     :  MIT
--
-- Maintainer  :  Jürgen Nicklisch <juergen.nicklisch@symbolian.net>
-- Stability   :  experimental
-- Portability :
--
-- | Optimize steps added to the bundle process for purescript.
--
-- This module takes as input the Javascript AST and applies optimizations like
-- uncurrying and inlining
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.Bundle.BundleOpt (
    uncurryFunc
) where

import Prelude.Compat
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

import Language.PureScript.Bundle.BundleTypes
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST


-- * Types

data FuncAdmin = FuncAdmin {
    moduleId  :: ModuleIdentifier,
    arity     :: Int,
    exported  :: Bool}
    deriving (Eq,Show)

type FuncAdminMap = M.Map String [FuncAdmin]

data FuncStats = FuncStats {
    moduleCount :: Int,
    moduleFunctions :: Int,
    uncurriedFuncs :: Int,
    exportedEntities :: Int,
    uncurriedFuncsExported :: Int,
    exportedForeignEntities :: Int,
    uncurriedForeignFuncsExported :: Int
} deriving (Eq,Show)

data FuncCollector = FuncCollector {
    adminMap :: FuncAdminMap,
    stats :: FuncStats
} deriving (Eq,Show)

-- * Constants

-- arbitrary suffix that gets mangled into an uncurried function's name
suffix :: String
suffix = "$_$_$"


-- * Functions

-- | Main function for uncurry optimization
uncurryFunc :: [Module] -> [ModuleIdentifier] -> [Module]
uncurryFunc modules entryPoints =
    -- add uncurried functions
    let (modulesWithUncurried,funcCollector) = foldr generateUncurried ([],emptyCollector) modules
    -- add exports for uncurried functions
        (modulesWithExports,funcCollector2)  = foldr (generateUncurriedExports entryPoints) ([],funcCollector) modulesWithUncurried
    -- replace saturated calls to calls to uncurried functions
        (modulesWithCalls, _funcCollector3)    = -- trace ("Uncurry Stats: " ++ (show (stats funcCollector2))) $
                                                foldr generateSaturedCalls ([],funcCollector2) modulesWithExports

    in  modulesWithCalls


-- | Generation of uncurried functions
generateUncurried :: Module -> ([Module],FuncCollector) -> ([Module],FuncCollector)
generateUncurried (Module moduleIdentifier mfp moduleElements) (modules, funcCollector) =
    let newCollector = funcCollector {adminMap = adminMap funcCollector,
                                      stats = (stats funcCollector){moduleCount = moduleCount (stats funcCollector) + 1}}
        (eles,funcCollector') = foldr (generateUncurriedEle moduleIdentifier) ([],newCollector) moduleElements
    in (Module moduleIdentifier mfp eles : modules, funcCollector')


-- |  Generate uncurried functions from curried functions
generateUncurriedEle :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncCollector) -> ([ModuleElement],FuncCollector)
generateUncurriedEle mid m@(Member jsNode sort name decl keys) (eles, funcCollector) -- a var decl
    | JSVariable _ (JSLOne varInitE) _ <-  jsNode
    , JSVarInitExpression (JSIdentifier _ia1 _funcName) varInit <- varInitE
    , JSVarInit _va1 funcE <- varInit
    , JSFunctionExpression _ _ _ idiL@(JSLOne _arg1) _ block <-  funcE
    =   let newCollector = funcCollector
                {adminMap = adminMap funcCollector,
                 stats = (stats funcCollector){moduleFunctions = moduleFunctions (stats funcCollector) + 1}}
        {-trace ("candidate: " ++ name ++ " para: " ++ show idi) $-}
        in case analyzeUncurriedPrim idiL block of
            Nothing -> (m : eles, newCollector)
            Just (argList,block') -> generateUncurried1 argList block' newCollector
  where
    generateUncurried1 :: JSCommaList JSIdent -> JSBlock -> FuncCollector -> ([ModuleElement],FuncCollector)
    generateUncurried1 newArgList block funcCollector'
        | JSVariable va1 (JSLOne varInitE) rb2 <-  jsNode
        , JSVarInitExpression (JSIdentifier ia1 _funcName) varInit <- varInitE
        , JSVarInit va2 _funcE <- varInit
        , JSFunctionExpression u1 u2 lb _ rb1 _ <-  decl
        = let newName       = name ++ suffix
              newDecl       = JSFunctionExpression u1 u2 lb newArgList rb1 block
              newVarIntro   = JSVarInitExpression (JSIdentifier ia1 newName) (JSVarInit va2 newDecl)
              newNode       = JSVariable va1 (JSLOne newVarIntro) rb2
              newAdmin      = FuncAdmin { moduleId = mid, arity = lengthJSCommaList newArgList, exported = False}
              newCollector  = funcCollector' {adminMap = addAdmin name newAdmin (adminMap funcCollector'),
                                stats = (stats funcCollector'){uncurriedFuncs = uncurriedFuncs (stats funcCollector') + 1}}
              in (m : Member newNode sort newName newDecl keys : eles, newCollector)
    generateUncurried1 _ _ funcCollector' = (m : eles, funcCollector')

generateUncurriedEle mid m@(Member jsNode sort name decl keys) (eles, funcCollector) -- a var decl
    | JSAssignStatement expr1 _u1 expr2 _u2  <-  jsNode
    , JSMemberDot (JSIdentifier _ia1 _exp)  _mi (JSIdentifier _ia2 _funcName)  <- expr1
    , JSFunctionExpression _ _ _ idiL@(JSLOne _arg1) _ block <-  expr2
    =   let newCollector = funcCollector
                {adminMap = adminMap funcCollector,
                 stats = (stats funcCollector){moduleFunctions = moduleFunctions (stats funcCollector) + 1}}
        {-trace ("candidate: " ++ name ++ " para: " ++ show idi) $-}
        in case analyzeUncurriedPrim idiL block of
            Nothing -> (m : eles, newCollector)
            Just (argList,block') -> generateUncurried2 argList block' newCollector
    where
      generateUncurried2 :: JSCommaList JSIdent -> JSBlock -> FuncCollector -> ([ModuleElement],FuncCollector)
      generateUncurried2 newArgList block funcCollector'
          | JSAssignStatement expr1 u1 _expr2 u2  <-  jsNode
          , JSMemberDot (JSIdentifier ia1 expo)  mi (JSIdentifier ia2 _funcName)  <- expr1
          , JSFunctionExpression uf1 uf2 lb _ rb1 _ <-  decl
          = let newName       = name ++ suffix
                newDecl       = JSFunctionExpression uf1 uf2 lb newArgList rb1 block
                newMemberDot  = JSMemberDot (JSIdentifier ia1 expo) mi (JSIdentifier ia2 newName)
                newNode       = JSAssignStatement newMemberDot u1 newDecl u2
                newAdmin      = FuncAdmin { moduleId = mid, arity = lengthJSCommaList newArgList, exported = False}
                newCollector  = funcCollector' {adminMap = addAdmin name newAdmin (adminMap funcCollector'),
                                  stats = (stats funcCollector'){uncurriedFuncs = uncurriedFuncs (stats funcCollector') + 1}}
                in (m : Member newNode sort newName newDecl keys : eles, newCollector)
      generateUncurried2 _ _ funcCollector' = (m : eles, funcCollector')
generateUncurriedEle _mid m (eles, funcCollector) = (m : eles, funcCollector)


analyzeUncurriedPrim :: JSCommaList JSIdent -> JSBlock -> Maybe (JSCommaList JSIdent, JSBlock)
analyzeUncurriedPrim idList decl
    | JSBlock _ [ef] _      <-  decl
    , JSReturn _ (Just ef2) _    <-  ef
    , JSFunctionExpression _ _fn _ (JSLOne idi) _ block <-  ef2
    = {-trace ("found deeper: " ++ show idList ++ " para: " ++ show idi) $-}
        analyzeUncurriedPrim (consJSCommaList idi idList) block
analyzeUncurriedPrim l block | lengthJSCommaList l > 1 = Just (l,block)
analyzeUncurriedPrim _ _ = Nothing

-- * Exports

-- | Generation of uncurried exports
generateUncurriedExports :: [ModuleIdentifier] -> Module -> ([Module],FuncCollector) -> ([Module],FuncCollector)
generateUncurriedExports entryPoints (Module moduleIdentifier mfp moduleElements) (modules, funcCollector) =
    if elem moduleIdentifier entryPoints
        then (Module moduleIdentifier mfp moduleElements : modules, funcCollector)
        else
            let (eles,funcCollector') = foldr (generateUncurriedExpo moduleIdentifier) ([],funcCollector) moduleElements
            in (Module moduleIdentifier mfp eles : modules, funcCollector')

generateUncurriedExpo :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncCollector) -> ([ModuleElement],FuncCollector)
generateUncurriedExpo mid (ExportsList l) (eles, funcCollector) =
    let (newExports,funcCollector') = foldr (generateUncurriedEx mid) ([],funcCollector) l
    in (ExportsList newExports : eles, funcCollector')
generateUncurriedExpo _mid other (eles, funcCollector) = (other : eles, funcCollector)

generateUncurriedEx :: ModuleIdentifier -> (ExportType, String, JSExpression, [Key])
    -> ([(ExportType, String, JSExpression, [Key])],FuncCollector) -> ([(ExportType, String, JSExpression, [Key])],FuncCollector)
generateUncurriedEx mid t@(RegularExport name1, name2, jSNode, [key]) (eles, funcCollector)
    | JSIdentifier ianno _name3 <-  jSNode
    = -- [(ExportType, String, JSStatement, [Key])]
        let newCollector  = funcCollector {stats = (stats funcCollector){exportedEntities = exportedEntities (stats funcCollector) + 1}}
        in case findAdminFor name2 mid (adminMap newCollector) of
            Nothing -> (t : eles, newCollector)
            Just admin ->
                let newName   = name2 ++ suffix
                    newAdmin  = admin {exported = True}
                    newNode   =  (JSIdentifier ianno newName)
                    newKeys   = [(fst key,newName)]
                    newExport = (RegularExport (name1 ++ suffix    ), newName, newNode, newKeys)
                    newCollector' =  newCollector {adminMap = replaceAdmin name2 admin newAdmin (adminMap newCollector),
                                      stats = (stats newCollector){uncurriedFuncsExported = uncurriedFuncsExported (stats newCollector) + 1}}
                in  (t : newExport : eles, newCollector')

generateUncurriedEx mid t@(ForeignReexport, name, jSNode, [key]) (eles, funcCollector)
    | JSMemberDot l m r <-  jSNode
    , JSIdentifier anno "$foreign" <-  l
    , JSIdentifier anno3 name2 <-  r
    = let newCollector  = funcCollector {stats = (stats funcCollector){exportedForeignEntities = exportedForeignEntities (stats funcCollector) + 1}}
      in case findAdminFor name2 mid (adminMap newCollector) of
            Nothing -> (t : eles, newCollector)
            Just admin ->
                let newName   = name ++ suffix
                    newAdmin  = admin {exported = True}
                    newNode   =  (JSMemberDot (JSIdentifier anno "$foreign") m
                                        (JSIdentifier anno3 newName))
                    newKeys   = [(fst key,newName)]
                    newExport = (ForeignReexport, newName, newNode, newKeys)
                    newCollector' =  newCollector {adminMap = replaceAdmin name2 admin newAdmin (adminMap newCollector),
                                      stats = (stats newCollector){uncurriedForeignFuncsExported = uncurriedForeignFuncsExported (stats newCollector) + 1}}
                in  (t : newExport : eles, newCollector')

generateUncurriedEx _mid t (eles, funcCollector) =
    {-trace ("export in unknown form: " ++ show t)-} (t : eles, funcCollector)

-- * Call replacement

-- | replace saturated calls to calls to uncurried functions
generateSaturedCalls :: Module -> ([Module],FuncCollector) -> ([Module],FuncCollector)
generateSaturedCalls (Module moduleIdentifier mfp moduleElements) (modules, funcCollector) =
    {- trace ("generateSaturedCalls: " ++ show moduleIdentifier) $ -}
    let (eles,funcCollector') = foldr (generateSaturedC moduleIdentifier imports) ([],funcCollector) moduleElements
    in (Module moduleIdentifier mfp eles : modules, funcCollector')
  where
    imports :: [(String, ModuleIdentifier)]
    imports = mapMaybe toImport moduleElements
      where
      toImport :: ModuleElement -> Maybe (String, ModuleIdentifier)
      toImport (Require _ nm (Right mid)) = Just (nm, mid)
      toImport _ = Nothing

-- |  Generate uncurried functions from curried functions
generateSaturedC :: ModuleIdentifier -> [(String, ModuleIdentifier)] -> ModuleElement ->
    ([ModuleElement],FuncCollector) -> ([ModuleElement],FuncCollector)
generateSaturedC mid imports (Member node' sort name decl keys) (eles, funcCollector) =
    let replaceSaturedNode = replaceSaturedStatement node'
        replaceSaturedDecl = replaceSaturedExpression decl
    in  (Member replaceSaturedNode sort name replaceSaturedDecl keys : eles, funcCollector)
  where
    replaceSaturedStatement :: JSStatement -> JSStatement
    replaceSaturedStatement (JSStatementBlock ann statements ann2 semi) =
        JSStatementBlock ann (map replaceSaturedStatement statements) ann2 semi
    replaceSaturedStatement inp@(JSBreak _ann _ident _semi) = inp
    replaceSaturedStatement (JSConstant ann expressions semi) =
        JSConstant ann (mapJSCommaList replaceSaturedExpression expressions) semi
    replaceSaturedStatement inp@(JSContinue _ _ _) = inp
    replaceSaturedStatement (JSDoWhile continue stmt while lb expr rb autosemi) =
        JSDoWhile continue (replaceSaturedStatement stmt) while lb (replaceSaturedExpression expr) rb autosemi
    replaceSaturedStatement (JSFor u1 u2 exprL1 u3 exprL2 u4 exprL3 u5 stmt) =
        JSFor u1 u2 (mapJSCommaList replaceSaturedExpression exprL1) u3 (mapJSCommaList replaceSaturedExpression exprL2) u4
            (mapJSCommaList replaceSaturedExpression exprL3) u5 (replaceSaturedStatement stmt)
    replaceSaturedStatement (JSForIn u1 u2 expr1 binOp expr2 u3 stmt) =
        JSForIn u1 u2 (replaceSaturedExpression expr1) binOp (replaceSaturedExpression expr2) u3 (replaceSaturedStatement stmt)
    replaceSaturedStatement (JSForVar u1 u2 u3 exprL1 u4 exprL2 u5 exprL3 u6 stmt) =
        JSForVar u1 u2 u3 (mapJSCommaList replaceSaturedExpression exprL1) u4 (mapJSCommaList replaceSaturedExpression exprL2) u5
            (mapJSCommaList replaceSaturedExpression exprL3) u6 (replaceSaturedStatement stmt)
    replaceSaturedStatement (JSForVarIn u1 u2 u3 expr1 u4 expr2 u5 stmt) =
        JSForVarIn u1 u2 u3 (replaceSaturedExpression expr1) u4 (replaceSaturedExpression expr2) u5
            (replaceSaturedStatement stmt)
    replaceSaturedStatement (JSFunction u1 u2 u3 u4 u5 block u6) =
        JSFunction u1 u2 u3 u4 u5 (replaceSaturedBlock block) u6
    replaceSaturedStatement (JSIf u1 u2 expr u3 stmt) =
        JSIf u1 u2 (replaceSaturedExpression expr) u3 (replaceSaturedStatement stmt)
    replaceSaturedStatement (JSIfElse u1 u2 expr u3 stmt1 u4 stmt2) =
        JSIfElse u1 u2 (replaceSaturedExpression expr) u3 (replaceSaturedStatement stmt1) u4 (replaceSaturedStatement stmt2)
    replaceSaturedStatement (JSLabelled u1 u2 stmt) =
        JSLabelled u1 u2 (replaceSaturedStatement stmt)
    replaceSaturedStatement inp@(JSEmptyStatement _) = inp
    replaceSaturedStatement (JSExpressionStatement expr u1) = JSExpressionStatement (replaceSaturedExpression expr) u1
    replaceSaturedStatement (JSAssignStatement expr1 u1 expr2 u2) =
        JSAssignStatement (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2) u2
    replaceSaturedStatement (JSMethodCall expr1 u1 exprL u2 u3) = -- TODO currently not used
        JSMethodCall (replaceSaturedExpression expr1) u1 (mapJSCommaList replaceSaturedExpression exprL) u2 u3
    replaceSaturedStatement (JSReturn u1 (Just expr) u2) = JSReturn u1 (Just (replaceSaturedExpression expr)) u2
    replaceSaturedStatement inp@(JSReturn _u1 Nothing _u2) = inp
    replaceSaturedStatement (JSSwitch u1 u2 expr u3 u4 switchParts u5 u6) =
        JSSwitch u1 u2 (replaceSaturedExpression expr) u3 u4 (map replaceSaturedSwitchPart switchParts) u5 u6
    replaceSaturedStatement (JSThrow u1 expr u2) =
        JSThrow u1 (replaceSaturedExpression expr) u2
    replaceSaturedStatement (JSTry u1 block tryCatch tryFinally) =
        JSTry u1 (replaceSaturedBlock block) (map replaceSaturedTryCatch tryCatch) (replaceSaturedTryFinally tryFinally)
    replaceSaturedStatement (JSVariable u1 exprL u2) = JSVariable u1 (mapJSCommaList replaceSaturedExpression exprL) u2
    replaceSaturedStatement (JSWhile u1 u2 expr u3 stmt) =
        JSWhile u1 u2 (replaceSaturedExpression expr) u3 (replaceSaturedStatement stmt)
    replaceSaturedStatement (JSWith u1 u2 expr u3 stmt u4) =
        JSWith u1 u2 (replaceSaturedExpression expr) u3 (replaceSaturedStatement stmt) u4

    replaceSaturedBlock (JSBlock u1 stmtL u2) = JSBlock u1 (map replaceSaturedStatement stmtL) u2

    replaceSaturedSwitchPart  (JSCase u1 expr u2 stmtL) =
        JSCase u1 (replaceSaturedExpression expr) u2 (map replaceSaturedStatement stmtL)
    replaceSaturedSwitchPart (JSDefault u1 u2 stmtL) =
        JSDefault u1 u2 (map replaceSaturedStatement stmtL)

    replaceSaturedTryCatch (JSCatch u1 u2 expr u3 block) =
        JSCatch u1 u2 (replaceSaturedExpression expr) u3 (replaceSaturedBlock block)
    replaceSaturedTryCatch (JSCatchIf u1 u2 expr1 u3 expr2 u4 block) =
        JSCatchIf u1 u2 (replaceSaturedExpression expr1) u3 (replaceSaturedExpression expr2) u4 (replaceSaturedBlock block)

    replaceSaturedTryFinally (JSFinally u1 block) = JSFinally u1 (replaceSaturedBlock block)
    replaceSaturedTryFinally JSNoFinally = JSNoFinally

    replaceSaturedExpression  inp@(JSIdentifier _u1 _u2) = inp
    replaceSaturedExpression  inp@(JSDecimal _u1 _u2) = inp
    replaceSaturedExpression  inp@(JSLiteral _u1 _u2) = inp
    replaceSaturedExpression  inp@(JSHexInteger _ _)  = inp
    replaceSaturedExpression  inp@(JSOctal _ _)       = inp
    replaceSaturedExpression  inp@(JSStringLiteral _ _)= inp
    replaceSaturedExpression  inp@(JSRegEx _ _)       = inp
    replaceSaturedExpression  (JSArrayLiteral u1 arrayElementL u2) =
        JSArrayLiteral u1 (map replaceSaturedArrayElement arrayElementL) u2
    replaceSaturedExpression  (JSAssignExpression expr1 u1 expr2) =
        JSAssignExpression (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2)
    replaceSaturedExpression  inp@(JSCallExpression _expr _u1 _exprL _u2) = -- Here we uncurry Calls
        mayUncurryCallExpression inp [] inp
        -- JSCallExpression (replaceSaturedExpression expr) u1 (mapJSCommaList replaceSaturedExpression exprL) u2
    replaceSaturedExpression  (JSCallExpressionDot expr1 u1 expr2) =
        JSCallExpressionDot (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2)
    replaceSaturedExpression (JSCallExpressionSquare expr1 u1 expr2 u2) =
        JSCallExpressionSquare (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2) u2
    replaceSaturedExpression (JSCommaExpression expr1 u1 expr2) =
        JSCommaExpression (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2)
    replaceSaturedExpression (JSExpressionBinary expr1 u1 expr2) =
        JSExpressionBinary (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2)
    replaceSaturedExpression (JSExpressionParen u1 expr u2) =
        JSExpressionParen u1 (replaceSaturedExpression expr) u2
    replaceSaturedExpression (JSExpressionPostfix expr u) = JSExpressionPostfix (replaceSaturedExpression expr) u
    replaceSaturedExpression (JSExpressionTernary expr1 u1 expr2 u2 expr3) =
        JSExpressionTernary (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2) u2
            (replaceSaturedExpression expr3)
    replaceSaturedExpression (JSFunctionExpression u1 u2 u3 u4 u5 block) =
        JSFunctionExpression u1 u2 u3 u4 u5 (replaceSaturedBlock block)
    replaceSaturedExpression (JSMemberDot expr1 u1 expr2) =
        JSMemberDot (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2)

    replaceSaturedExpression (JSMemberExpression expr u1 exprL u2) =
        JSMemberExpression (replaceSaturedExpression expr) u1 (mapJSCommaList replaceSaturedExpression exprL) u2
    replaceSaturedExpression (JSMemberNew u1 expr u2 exprL u3) =
        JSMemberNew u1 (replaceSaturedExpression expr) u2 (mapJSCommaList replaceSaturedExpression exprL) u3
    replaceSaturedExpression (JSMemberSquare expr1 u1 expr2 u2) =
        JSMemberSquare (replaceSaturedExpression expr1) u1 (replaceSaturedExpression expr2) u2
    replaceSaturedExpression (JSNewExpression u1 expr) =
        JSNewExpression u1 (replaceSaturedExpression expr)
    replaceSaturedExpression (JSObjectLiteral u1 objectPropertyL u2) =
        JSObjectLiteral u1 (mapJSCommaTrailingList replaceSaturedObjectProperty objectPropertyL) u2
    replaceSaturedExpression (JSUnaryExpression u1 expr) =
        JSUnaryExpression u1 (replaceSaturedExpression expr)
    replaceSaturedExpression (JSVarInitExpression expr varInitializer) =
        JSVarInitExpression  (replaceSaturedExpression expr) (replaceSaturedVarInitializer varInitializer)

    replaceSaturedArrayElement (JSArrayElement expr) = JSArrayElement (replaceSaturedExpression expr)
    replaceSaturedArrayElement inp@(JSArrayComma _anno) = inp

    replaceSaturedObjectProperty (JSPropertyAccessor u1 u2 u3 exprL u4 block) =
        JSPropertyAccessor u1 u2 u3 (map replaceSaturedExpression exprL) u4 (replaceSaturedBlock block)
    replaceSaturedObjectProperty (JSPropertyNameandValue u1 u2 exprL) =
        JSPropertyNameandValue u1 u2 (map replaceSaturedExpression exprL)

    replaceSaturedVarInitializer (JSVarInit u1 expr) = JSVarInit u1 (replaceSaturedExpression expr)
    replaceSaturedVarInitializer JSVarInitNone = JSVarInitNone

    mayUncurryCallExpression :: JSExpression -> [JSExpression] -> JSExpression -> JSExpression
    mayUncurryCallExpression expr args original  -- One more argument
        | JSCallExpression cexpr _cu1 cexprL _cu2 <- expr
        , JSLOne arg <- cexprL
        = mayUncurryCallExpression cexpr (replaceSaturedExpression arg : args) original
    mayUncurryCallExpression expr [] _original   -- Empty effect arg left untouched
        | JSCallExpression cexpr cu1 cexprL cu2 <- expr
        , JSLNil <- cexprL
        = JSCallExpression (replaceSaturedExpression cexpr) cu1 cexprL cu2
    mayUncurryCallExpression expr args original
        | JSMemberExpression mexpr mu1 mexprL mu2 <- expr
        , JSIdentifier iu1 funcName <- mexpr
        , JSLOne arg1 <- mexprL
        =   let newName = funcName ++ suffix
                arityFound = 1 + length args
            in case findAdminFor funcName mid (adminMap funcCollector) of
                Nothing -> if arityFound > 1
                                then {-trace ("replace candidate: not in map: " ++ funcName) $-}
                                        continueUncurry original
                                else continueUncurry original
                Just admin ->
                    if arity admin <= arityFound
                        then {-trace ("!*" ++ show arityFound) $-}
                            if arity admin == arityFound
                                then
                                    let argList = toJSCommaList (reverse (replaceSaturedExpression arg1 : args))
                                    in JSMemberExpression (JSIdentifier iu1 newName) mu1 argList mu2
                                else
                                    let newArgs     = replaceSaturedExpression arg1 : args
                                        argList     = toJSCommaList (reverse (take (arity admin) newArgs))
                                        mExpr       = JSMemberExpression (JSIdentifier iu1 newName) mu1 argList mu2
                                        extraArgs   = reverse $ drop (arity admin) newArgs
                                        callFunc arg ex = JSCallExpression ex JSNoAnnot (JSLOne arg) JSNoAnnot
                                    in -- trace ("special In: " ++ show (arity admin) ++ " " ++ show arityFound ++ " " ++ newName) $
                                        foldr callFunc mExpr extraArgs
                        else {-trace ("no replace!!! " ++ show (arity admin) ++ " " ++ show arityFound ++ " " ++ newName) $-}
                             continueUncurry original
    mayUncurryCallExpression expr args original
        | JSMemberExpression mexpr mu1 mexprL mu2 <- expr
        , JSMemberDot l m r <-  mexpr
        , JSIdentifier _ scope <-  l
        , JSIdentifier a1 funcName <-  r
        , JSLOne arg1 <- mexprL
        = let realMod = case lookup scope imports of
                            Nothing -> mid
                            Just moduleIdentifier -> moduleIdentifier
              newName = funcName ++ suffix
              arityFound = 1 + length args
          in case findAdminFor funcName realMod (adminMap funcCollector) of
                Nothing -> if arityFound > 1
                                then {-trace ("replace candidate: not in map: " ++ funcName) $-}
                                        continueUncurry original
                                else continueUncurry original
                Just admin ->
                    if arity admin <= arityFound
                        then {-trace ("!!*" ++ show (arity admin)) $-}
                            if arity admin == arityFound
                                then
                                    let argList = toJSCommaList (reverse (replaceSaturedExpression arg1 : args))
                                    in JSMemberExpression (JSMemberDot l m (JSIdentifier a1 newName)) mu1 argList mu2
                                else
                                    let newArgs     = replaceSaturedExpression arg1 : args
                                        argList     = toJSCommaList (reverse (take (arity admin) newArgs))
                                        mExpr       = JSMemberExpression (JSMemberDot l m (JSIdentifier a1 newName)) mu1 argList mu2
                                        extraArgs   = reverse $ drop (arity admin) newArgs
                                        callFunc arg ex = JSCallExpression ex JSNoAnnot (JSLOne arg) JSNoAnnot
                                    in {- trace ("special In: " ++ show (arity admin) ++ " " ++ show arityFound ++ " " ++ newName)  $ -}
                                        foldr callFunc mExpr extraArgs
                        else {-trace ("no replace!!! " ++ show (arity admin) ++ " " ++ show arityFound ++ " " ++ newName)-}
                            continueUncurry original
    mayUncurryCallExpression _expr _args original = continueUncurry original

    continueUncurry exprC
            | JSCallExpression expr u1 exprL u2 <- exprC
            = JSCallExpression (replaceSaturedExpression expr) u1 (mapJSCommaList replaceSaturedExpression exprL) u2
    continueUncurry _expr = error "BundleOpt>>continueUncurry: Impossible!"

generateSaturedC _ _ e (eles, funcCollector) = (e : eles, funcCollector)

-- * Admin
emptyStats :: FuncStats
emptyStats = FuncStats {
    moduleCount = 0,
    moduleFunctions = 0,
    uncurriedFuncs = 0,
    exportedEntities = 0,
    uncurriedFuncsExported = 0,
    exportedForeignEntities = 0,
    uncurriedForeignFuncsExported = 0
}

emptyCollector :: FuncCollector
emptyCollector = FuncCollector {
    adminMap = M.empty,
    stats = emptyStats
}

addAdmin :: String -> FuncAdmin -> FuncAdminMap ->  FuncAdminMap
addAdmin funcName funcAdmin funcAdminMap = M.insertWith (++) funcName [funcAdmin] funcAdminMap

replaceAdmin :: String -> FuncAdmin -> FuncAdmin -> FuncAdminMap ->  FuncAdminMap
replaceAdmin funcName oldFuncAdmin newFuncAdmin funcAdminMap = M.adjust replaceFunc funcName funcAdminMap
    where
        replaceFunc [o] | o == oldFuncAdmin =  [newFuncAdmin]
        replaceFunc l@(_hd:_) = filter (\e -> e /= oldFuncAdmin) l ++  [newFuncAdmin]
        replaceFunc [] = error "BundleOpt>>replaceAdmin: Impossible with FuncAdmin"

findAdminFor :: String -> ModuleIdentifier -> FuncAdminMap -> Maybe FuncAdmin
findAdminFor name mid funcAdminMap =
    case M.lookup name funcAdminMap of
        Nothing -> Nothing
        Just li -> case filter (\fa -> moduleName (moduleId fa) == moduleName mid) li of
                        [] -> Nothing
                        [e] -> Just e
                        _ -> error "BundleOpt>>findAdminFor: Impossible with Export"
