-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.BundleOpt
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

module Language.PureScript.BundleOpt (
    uncurryFunc
) where

import Debug.Trace
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

import Language.PureScript.BundleTypes
import Language.JavaScript.Parser.AST

-- * Types

data FuncAdmin = FuncAdmin
    { moduleId  :: ModuleIdentifier
    , arity     :: Int
    , exported  :: Bool}
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
    -- replace satured calls to calls to uncurried functions
        (modulesWithCalls, _funcCollector3)    = trace ("Uncurry Stats: " ++ (show (stats funcCollector2))) $
                                                foldr generateSaturedCalls ([],funcCollector2) modulesWithExports

    in  modulesWithCalls


-- | Generation of uncurried functions
generateUncurried :: Module -> ([Module],FuncCollector) -> ([Module],FuncCollector)
generateUncurried (Module moduleIdentifier moduleElements) (modules, funcCollector) =
    let newCollector = funcCollector {adminMap = adminMap funcCollector,
                                      stats = (stats funcCollector){moduleCount = moduleCount (stats funcCollector) + 1}}
        (eles,funcCollector') = foldr (generateUncurriedEle moduleIdentifier) ([],newCollector) moduleElements
    in (Module moduleIdentifier eles : modules, funcCollector')

-- |  Generate uncurried functions from curried functions
generateUncurriedEle :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncCollector) -> ([ModuleElement],FuncCollector)
generateUncurriedEle mid m@(Member _jSNode sort _name [decl] _keys) (eles, funcCollector) -- a var decl
    | JSFunctionExpression fn _names _lb [parameter] _rb block <- node decl
    , JSLiteral "function" <- node fn
    , JSIdentifier idi <- node parameter
            =   let newCollector = funcCollector
                        {adminMap = adminMap funcCollector,
                         stats = (stats funcCollector){moduleFunctions = moduleFunctions (stats funcCollector) + 1}}
                {-trace ("candidate: " ++ name ++ " para: " ++ show idi) $-}
                in case analyzeUncurriedPrim [idi] block of
                    Nothing -> (m : eles, newCollector)
                    Just (argList,block') -> if not sort
                                                then {-trace ("generateFor: " ++ name) $-}
                                                    generateUncurried1 argList block' m newCollector
                                                else {-trace ("generateFor2: " ++ name) $-}
                                                    generateUncurried2 argList block' m newCollector
  where
    generateUncurried1 :: [String] -> JSNode -> ModuleElement -> FuncCollector -> ([ModuleElement],FuncCollector)
    generateUncurried1 argList block (Member jsNode typ name2 [decl2] keys) funcCollector'
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl2
        , JSVariables var [ varIntro ] rb2 <- node jsNode
        , JSVarDecl _declN (eq : _decl3) <- node varIntro
        = let newName = name2 ++ suffix
              newArgList    = intersperse (nt (JSLiteral ","))
                                    $ map (nt . JSIdentifier)
                                        argList
              newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
              newVarIntro   = NN $ JSVarDecl (sp $ JSIdentifier newName) (eq : [newDecl])
              newNode       = NN $ JSVariables var [newVarIntro] rb2
              newAdmin      = FuncAdmin { moduleId = mid, arity = length argList, exported = False}
              newCollector  = funcCollector' {adminMap = addAdmin name2 newAdmin (adminMap funcCollector'),
                                stats = (stats funcCollector'){uncurriedFuncs = uncurriedFuncs (stats funcCollector') + 1}}
              in (m : Member newNode typ newName [newDecl] keys : eles, newCollector)
    generateUncurried1 _ _ _ funcCollector' = (m : eles, funcCollector')

    generateUncurried2 :: [String] -> JSNode -> ModuleElement -> FuncCollector -> ([ModuleElement],FuncCollector)
    generateUncurried2 argList block (Member jsNode typ name1 [decl2] keys) funcCollector'
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl2
        , JSExpression (e : op : _decl3) <- node jsNode
        =   let newName = name1 ++ suffix
            in case setAccessor (node e) newName of
                Nothing -> (m : eles, funcCollector')
                Just newE ->
                    let newArgList    = intersperse (nt (JSLiteral ","))
                                            $ map (nt . JSIdentifier)
                                                argList
                        newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
                        newNode       = NN $ JSExpression (NN newE : op : [newDecl])
                        newAdmin      = FuncAdmin { moduleId = mid, arity = length argList, exported = False}
                        newCollector  = funcCollector' {adminMap = addAdmin name1 newAdmin (adminMap funcCollector'),
                                          stats = (stats funcCollector'){uncurriedFuncs = uncurriedFuncs (stats funcCollector') + 1}}
                    in (m : Member newNode typ newName [newDecl] keys : eles, newCollector)
    generateUncurried2 _ _ _ funcCollector' = (m : eles, funcCollector')
generateUncurriedEle _mid m (eles, funcCollector) = (m : eles, funcCollector)

analyzeUncurriedPrim :: [String] -> JSNode -> Maybe ([String], JSNode)
analyzeUncurriedPrim idList decl
    | JSBlock _ [ef] _      <- node decl
    , JSReturn _ [ef2] _    <- node ef
    , JSExpression [ef3]    <- node ef2
    , JSFunctionExpression fn _names _lb [parameter] _rb block <- node ef3
    , JSLiteral "function"  <- node fn
    , JSIdentifier idi      <- node parameter
    = {-trace ("found deeper: " ++ show idList ++ " para: " ++ show idi) $-}
        analyzeUncurriedPrim (idi : idList) block
analyzeUncurriedPrim l@(_a:_b:_) block = Just (reverse l,block)
analyzeUncurriedPrim _ _ = Nothing

-- * Exports

-- | Generation of uncurried exports
generateUncurriedExports :: [ModuleIdentifier] -> Module -> ([Module],FuncCollector) -> ([Module],FuncCollector)
generateUncurriedExports entryPoints (Module moduleIdentifier moduleElements) (modules, funcCollector) =
    if elem moduleIdentifier entryPoints
        then (Module moduleIdentifier moduleElements : modules, funcCollector)
        else
            let (eles,funcCollector') = foldr (generateUncurriedExpo moduleIdentifier) ([],funcCollector) moduleElements
            in (Module moduleIdentifier eles : modules, funcCollector')

generateUncurriedExpo :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncCollector) -> ([ModuleElement],FuncCollector)
generateUncurriedExpo mid (ExportsList l) (eles, funcCollector) =
    let (newExports,funcCollector') = foldr (generateUncurriedEx mid) ([],funcCollector) l
    in (ExportsList newExports : eles, funcCollector')
generateUncurriedExpo _mid other (eles, funcCollector) = (other : eles, funcCollector)

generateUncurriedEx :: ModuleIdentifier -> (ExportType, String, JSNode, [Key])
    -> ([(ExportType, String, JSNode, [Key])],FuncCollector) -> ([(ExportType, String, JSNode, [Key])],FuncCollector)
generateUncurriedEx mid t@(RegularExport name1, name2, jSNode, [key]) (eles, funcCollector)
    | JSIdentifier _name3 <- node jSNode
    = -- [(ExportType, String, JSNode, [Key])]
        let newCollector  = funcCollector {stats = (stats funcCollector){exportedEntities = exportedEntities (stats funcCollector) + 1}}
        in case findAdminFor name2 mid (adminMap newCollector) of
            Nothing -> (t : eles, newCollector)
            Just admin ->
                let newName   = name2 ++ suffix
                    newAdmin  = admin {exported = True}
                    newNode   = nt (JSIdentifier newName)
                    newKeys   = [(fst key,newName)]
                    newExport = (RegularExport (name1 ++ suffix    ), newName, newNode, newKeys)
                    newCollector' =  newCollector {adminMap = replaceAdmin name2 admin newAdmin (adminMap newCollector),
                                      stats = (stats newCollector){uncurriedFuncsExported = uncurriedFuncsExported (stats newCollector) + 1}}
                in  (t : newExport : eles, newCollector')

generateUncurriedEx mid t@(ForeignReexport, name, jSNode, [key]) (eles, funcCollector)
    | JSMemberDot [l] m r <- node jSNode
    , JSIdentifier "$foreign" <- node l
    , JSLiteral "." <- node m
    , JSIdentifier name2 <- node r
    = let newCollector  = funcCollector {stats = (stats funcCollector){exportedForeignEntities = exportedForeignEntities (stats funcCollector) + 1}}
      in case findAdminFor name2 mid (adminMap newCollector) of
            Nothing -> (t : eles, newCollector)
            Just admin ->
                let newName   = name ++ suffix
                    newAdmin  = admin {exported = True}
                    newNode   = NN (JSMemberDot [nt (JSIdentifier "$foreign")] (nt (JSLiteral "."))
                                        (nt (JSIdentifier newName)))
                    newKeys   = [(fst key,newName)]
                    newExport = (ForeignReexport, newName, newNode, newKeys)
                    newCollector' =  newCollector {adminMap = replaceAdmin name2 admin newAdmin (adminMap newCollector),
                                      stats = (stats newCollector){uncurriedForeignFuncsExported = uncurriedForeignFuncsExported (stats newCollector) + 1}}
                in  (t : newExport : eles, newCollector')

generateUncurriedEx _mid t (eles, funcCollector) =
    trace ("export in unknown form: " ++ show t) (t : eles, funcCollector)

-- * Call replacement

-- | replace satured calls to calls to uncurried functions
generateSaturedCalls :: Module -> ([Module],FuncCollector) -> ([Module],FuncCollector)
generateSaturedCalls (Module moduleIdentifier moduleElements) (modules, funcCollector) =
    {- trace ("generateSaturedCalls: " ++ show moduleIdentifier) $ -}
    let (eles,funcCollector') = foldr (generateSaturedC moduleIdentifier imports) ([],funcCollector) moduleElements
    in (Module moduleIdentifier eles : modules, funcCollector')
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
generateSaturedC mid imports (Member jSNode' sort name decls keys) (eles, funcCollector) =
    let replaceSaturedNode = rscJS jSNode'
        replaceSaturedDecls = map rscJS decls
    in  (Member replaceSaturedNode sort name replaceSaturedDecls keys : eles, funcCollector)
  where
    rscJS :: JSNode -> JSNode
    rscJS (NN node') = (NN (rsc node'))
    rscJS (NT node' tokenPosn commentAnnotations) = (NT (rsc node') tokenPosn commentAnnotations)

    -- | AST traversal to replace satured function calls
    rsc :: Node -> Node
    rsc (JSArguments jSNodeL jSNodesM jSNodeR) =
        JSArguments (rscJS jSNodeL) (mapReplace jSNodesM) (rscJS jSNodeR)
    rsc (JSArrayLiteral jSNodeL jSNodesM jSNodeR) =
        JSArrayLiteral (rscJS jSNodeL) (mapReplace jSNodesM) (rscJS jSNodeR)
    rsc (JSBlock jSNodesL jSNodesM jSNodesR) =
        JSBlock (mapReplace jSNodesL) (mapReplace jSNodesM) (mapReplace jSNodesR)
    rsc (JSBreak jSNodeL jSNodesM jSNodeR) =
        JSBreak (rscJS jSNodeL) (mapReplace jSNodesM) (rscJS jSNodeR)
    rsc (JSCallExpression string jSNodesL jSNodesM jSNodesR) =
         JSCallExpression string (mapReplace jSNodesL) (mapReplace jSNodesM) (mapReplace jSNodesR)
    rsc (JSCase jSNodeL jSNodeM jSNodeR jSNodesE) =
        JSCase (rscJS jSNodeL) (rscJS jSNodeM) (rscJS jSNodeR) (mapReplace jSNodesE)
    rsc (JSCatch jSNodeL jSNodeM jSNodeR jSNodesE1 jSNodeE2 jSNodeE3) =
        JSCatch (rscJS jSNodeL) (rscJS jSNodeM) (rscJS jSNodeR) (mapReplace jSNodesE1)
                (rscJS jSNodeE2) (rscJS jSNodeE3)
    rsc (JSContinue jSNodeL jSNodesM jSNodeR) =
         JSContinue (rscJS jSNodeL) (mapReplace jSNodesM) (rscJS jSNodeR)
    rsc (JSDefault jSNodeL jSNodeM jSNodesR) =
        JSDefault (rscJS jSNodeL) (rscJS jSNodeM) (mapReplace jSNodesR)
    rsc (JSDoWhile jSNode1 jSNode2 jSNode3 jSNode4 jSNode5 jSNode6 jSNode7) =
        JSDoWhile (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3) (rscJS jSNode4)
                    (rscJS jSNode5) (rscJS jSNode6) (rscJS jSNode7)
    rsc (JSElision jSNode) =
        JSElision (rscJS jSNode)
    rsc (JSExpression jSNodes) =
        JSExpression (mapReplace jSNodes)
    rsc (JSExpressionBinary string jSNodesL jSNodeM jSNodesR) =
        JSExpressionBinary string (mapReplace jSNodesL) (rscJS jSNodeM) (mapReplace jSNodesR)
    rsc (JSExpressionParen jSNodeL jSNodeM jSNodeR) =
        JSExpressionParen (rscJS jSNodeL) (rscJS jSNodeM) (rscJS jSNodeR)
    rsc (JSExpressionPostfix string jSNodesL jSNodeR) =
        JSExpressionPostfix string (mapReplace jSNodesL) (rscJS jSNodeR)
    rsc (JSExpressionTernary jSNodesL jSNodeM jSNodesR jSNodeE1 jSNodesE2) =
        JSExpressionTernary (mapReplace jSNodesL) (rscJS jSNodeM)(mapReplace jSNodesR)
            (rscJS jSNodeE1)(mapReplace jSNodesE2)
    rsc (JSFinally jSNodeL jSNodeR) =
        JSFinally (rscJS jSNodeL) (rscJS jSNodeR)
    rsc (JSFor jSNode1 jSNode2 jSNodes3 jSNode4 jSNodes5 jSNode6 jSNodes7 jSNode8 jSNode9) =
        JSFor (rscJS jSNode1) (rscJS jSNode2) (mapReplace jSNodes3)
                (rscJS jSNode4) (mapReplace jSNodes5)
                (rscJS jSNode6) (mapReplace jSNodes7)
                (rscJS jSNode8) (rscJS jSNode9)
    rsc (JSForIn jSNode1 jSNode2 jSNodes3 jSNode4 jSNode5 jSNode6 jSNode7) =
        JSForIn (rscJS jSNode1) (rscJS jSNode2) (mapReplace jSNodes3)
            (rscJS jSNode4) (rscJS jSNode5)
            (rscJS jSNode6) (rscJS jSNode7)
    rsc (JSForVar jSNode1 jSNode2 jSNode3 jSNodes4 jSNode5 jSNodes6 jSNode7 jSNodes8 jSNode9 jSNode10) =
        JSForVar (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
                (mapReplace jSNodes4) (rscJS jSNode5)
                (mapReplace jSNodes6) (rscJS jSNode7)
                (mapReplace jSNodes8) (rscJS jSNode9) (rscJS jSNode10)
    rsc (JSForVarIn jSNode1 jSNode2 jSNode3 jSNode4 jSNode5 jSNode6 jSNode7 jSNode8) =
        JSForVarIn (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
            (rscJS jSNode4) (rscJS jSNode5)
            (rscJS jSNode6) (rscJS jSNode7)
            (rscJS jSNode8)
    rsc (JSFunction jSNode1 jSNode2 jSNode3 jSNodes4 jSNode5 jSNode6) =
        JSFunction (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
            (mapReplace jSNodes4) (rscJS jSNode5)
            (rscJS jSNode6)
    rsc (JSFunctionExpression jSNode1 jSNodes2 jSNode3 jSNodes4 jSNode5 jSNode6) =
        JSFunctionExpression (rscJS jSNode1) (mapReplace jSNodes2) (rscJS jSNode3)
            (mapReplace jSNodes4) (rscJS jSNode5)
            (rscJS jSNode6)
    rsc (JSIf jSNode1 jSNode2 jSNode3 jSNode4 jSNodes5 jSNodes6) =
        JSIf (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
            (rscJS jSNode4) (mapReplace jSNodes5)
            (mapReplace jSNodes6)
    rsc (JSLabelled jSNode1 jSNode2 jSNode3) =
        JSLabelled (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
    rsc (JSMemberDot jSNodes1 jSNode2 jSNode3) =
        JSMemberDot (mapReplace jSNodes1) (rscJS jSNode2) (rscJS jSNode3)
    rsc (JSMemberSquare jSNodes1 jSNode2 jSNode3 jSNode4) =
        JSMemberSquare (mapReplace jSNodes1) (rscJS jSNode2) (rscJS jSNode3)
                (rscJS jSNode4)
    rsc (JSObjectLiteral jSNode1 jSNodes2 jSNode3) =
        JSObjectLiteral (rscJS jSNode1) (mapReplace jSNodes2) (rscJS jSNode3)
    rsc (JSOperator jSNode) =
        JSOperator (rscJS jSNode)
    rsc (JSPropertyAccessor jSNode1 jSNode2 jSNode3 jSNodes4 jSNode5 jSNode6) =
        JSPropertyAccessor (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
            (mapReplace jSNodes4) (rscJS jSNode5)
            (rscJS jSNode6)
    rsc (JSPropertyNameandValue jSNode1 jSNode2 jSNodes3) =
        JSPropertyNameandValue (rscJS jSNode1) (rscJS jSNode2) (mapReplace jSNodes3)
    rsc (JSReturn jSNode1 jSNodes2 jSNode3) =
        JSReturn (rscJS jSNode1) (mapReplace jSNodes2) (rscJS jSNode3)
    rsc (JSSourceElementsTop jSNodes) =
        JSSourceElementsTop (mapReplace jSNodes)
    rsc (JSSwitch jSNode1 jSNode2 jSNode3 jSNode4 jSNode5) =
        JSSwitch (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
            (rscJS jSNode4) (rscJS jSNode5)
    rsc (JSThrow jSNode1 jSNode2) =
        JSThrow (rscJS jSNode1) (rscJS jSNode2)
    rsc (JSTry jSNode1 jSNode2 jSNodes3) =
        JSTry (rscJS jSNode1) (rscJS jSNode2) (mapReplace jSNodes3)
    rsc (JSUnary string jSNode) =
        JSUnary string (rscJS jSNode)
    rsc (JSVarDecl jSNode jSNodes) =
        JSVarDecl (rscJS jSNode) (mapReplace jSNodes)
    rsc (JSVariables jSNode1 jSNodes2 jSNode3) =
        JSVariables (rscJS jSNode1) (mapReplace jSNodes2) (rscJS jSNode3)
    rsc (JSWhile jSNode1 jSNode2 jSNode3 jSNode4 jSNode5) =
        JSWhile (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
            (rscJS jSNode4) (rscJS jSNode5)
    rsc (JSWith jSNode1 jSNode2 jSNode3 jSNode4 jSNodes5) =
        JSWith (rscJS jSNode1) (rscJS jSNode2) (rscJS jSNode3)
            (rscJS jSNode4) (mapReplace jSNodes5)
    rsc e = e

    mapReplace :: [JSNode] -> [JSNode]
    mapReplace [] = []
    mapReplace nodes@(n1:n2:n3:rest)
        |  JSIdentifier name' <- node n1
        ,  JSArguments _al arg1 _ar <- node n2
        ,  JSCallExpression _str _cl [cm] _cr <- node n3
        ,  JSArguments _a2l arg2 _a2r <- node cm
        = {-trace ("replace candidate: " ++ name' ++ " " ++ intercalate " " (map showStripped nodes)) $-}
            case findAdminFor name' mid (adminMap funcCollector) of
                Nothing -> {-trace ("replace candidate: not in map: " ++ name') $-}
                                (rscJS n1) : mapReplace (n2:n3:rest)
                Just admin ->
                    let moreCallArgs = getCallArgs rest
                        newName = name' ++ suffix
                        arityFound = 2 + length moreCallArgs
                    in if arity admin <= arityFound
                        then {-trace ("!*" ++ show (arity admin)) $-}
                            let newNodes = generateNewCall (sp $ JSIdentifier newName)
                                                (node n2) (arg1: arg2 : take (arity admin - 2) moreCallArgs)
                            in mapReplace newNodes ++ drop (1 + arity admin) nodes
                        else {-trace ("no replace!!! " ++ show (arity admin) ++ " " ++ show (2 + length moreCallArgs)) $-}
                            (rscJS n1) : mapReplace (n2:n3:rest)

    mapReplace nodes@(n1:n2:n3:rest)
        |  JSMemberDot [l] m r <- node n1
        ,  JSIdentifier scope <- node l
        ,  JSIdentifier name' <- node r
        ,  JSArguments _al arg1 _ar <- node n2
        ,  JSCallExpression _str _cl [cm] _cr <- node n3
        ,  JSArguments _a2l arg2 _a2r <- node cm
        = {-trace ("replace candidate: " ++ name' ++ " " ++  intercalate " " (map showStripped nodes)) $-}
            let realMod = case lookup scope imports of
                            Nothing -> mid
                            Just moduleIdentifier -> moduleIdentifier
            in case findAdminFor name' realMod (adminMap funcCollector) of
                Nothing -> {-trace ("replace candidate: not in map: " ++ name') $-}
                                (rscJS n1) : mapReplace (n2:n3:rest)
                Just admin ->
                    let moreCallArgs = getCallArgs rest
                        newName = name' ++ suffix
                        arityFound = 2 + length moreCallArgs
                    in if arity admin <= arityFound
                        then {-trace ("!*" ++ show (arity admin)) $-}
                            let newNodes = generateNewCall (NN $ JSMemberDot [l] m (nt (JSIdentifier newName)))
                                                (node n2) (arg1: arg2 : take (arity admin - 2) moreCallArgs)
                            in mapReplace newNodes ++ drop (1 + arity admin) nodes
                        else {-trace ("no replace!!! " ++ show (arity admin) ++ " " ++ show (2 + length moreCallArgs)) $-}
                            (rscJS n1) : mapReplace (n2:n3:rest)

    mapReplace (hd:tl) = (rscJS hd) : mapReplace tl

generateSaturedC _ _ e (eles, funcCollector) = (e : eles, funcCollector)

getCallArgs :: [JSNode] -> [[JSNode]]
getCallArgs (hd:tl)
    |  JSCallExpression _str _cl [cm] _cr <- node hd
    ,  JSArguments _a2l arg _a2r <- node cm
    = arg : getCallArgs tl
getCallArgs _ = []

generateNewCall :: JSNode -> Node -> [[JSNode]] -> [JSNode]
generateNewCall idNode (JSArguments al _old ar) argNodes
    = let argList = concat $ intersperse [(sp (JSLiteral ","))] argNodes
      in [idNode, NN (JSArguments al argList ar)]
generateNewCall _idNode _args _argNodes
    = error "BundleOpt>>generateNewCall: Impossible match error."

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
        replaceFunc l@(_hd:_) = (filter (\e -> e /= oldFuncAdmin) l) ++  [newFuncAdmin]
        replaceFunc [] = error "BundleOpt>>replaceAdmin: Impossible with FuncAdmin"

findAdminFor :: String -> ModuleIdentifier -> FuncAdminMap -> Maybe FuncAdmin
findAdminFor name mid funcAdminMap =
    case M.lookup name funcAdminMap of
        Nothing -> Nothing
        Just li -> case filter (\fa -> moduleName (moduleId fa) == moduleName mid) li of
                        [] -> Nothing
                        [e] -> Just e
                        _ -> error "BundleOpt>>findAdminFor: Impossible with Export"
