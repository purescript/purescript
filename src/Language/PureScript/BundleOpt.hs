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
import Data.List (intersperse, intercalate)
import qualified Data.Map as M

import Language.PureScript.BundleTypes
import Language.JavaScript.Parser.AST

suffix :: String
suffix = "$_$_$"

-- | Main function for uncurry optimization
uncurryFunc :: [Module] -> [Module]
uncurryFunc modules =
    -- add uncurried functions
    let (modulesWithUncurried,adminMap) = foldr generateUncurried ([],M.empty) modules
    -- add exports for uncurried functions
        (modulesWithExports,adminMap2)  = foldr generateUncurriedExports ([],adminMap) modulesWithUncurried
    -- replace satured calls to calls to uncurried functions
        (modulesWithCalls, adminMap3)    = foldr generateSaturedCalls ([],adminMap2) modulesWithExports
    in  trace (show adminMap3) $ modulesWithCalls

-- * Admin
data FuncAdmin = FuncAdmin
    { moduleId  :: ModuleIdentifier
    , arity     :: Int
    , exported  :: Bool}
    deriving (Eq,Show)

type FuncAdminMap = M.Map String [FuncAdmin]

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

-- * Functions

-- | Generation of uncurried functions
generateUncurried :: Module -> ([Module],FuncAdminMap) -> ([Module],FuncAdminMap)
generateUncurried (Module moduleIdentifier moduleElements) (modules, adminMap) =
        let (eles,adminMap') = foldr (generateUncurriedEle moduleIdentifier) ([],adminMap) moduleElements
        in (Module moduleIdentifier eles : modules, adminMap')

-- |  Generate uncurried functions from curried functions
generateUncurriedEle :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncAdminMap) -> ([ModuleElement],FuncAdminMap)
generateUncurriedEle mid m@(Member _jSNode sort name [decl] _keys) (eles, adminMap) -- a var decl
    | JSFunctionExpression fn _names _lb [parameter] _rb block <- node decl
    , JSLiteral "function" <- node fn
    , JSIdentifier idi <- node parameter
        = trace ("candidate: " ++ name ++ " para: " ++ show idi) $
            case analyzeUncurriedPrim [idi] block of
                Nothing -> (m : eles, adminMap)
                Just (argList,block') -> if not sort
                                            then trace ("generateFor: " ++ name) $ generateUncurried1 argList block' m
                                            else trace ("generateFor2: " ++ name) $ generateUncurried2 argList block' m
  where
    generateUncurried1 :: [String] -> JSNode -> ModuleElement -> ([ModuleElement],FuncAdminMap)
    generateUncurried1 argList block (Member jsNode typ name2 [decl2] keys)
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
              in (m : Member newNode typ newName [newDecl] keys : eles, addAdmin name2 newAdmin adminMap)
    generateUncurried1 _ _ _ = (m : eles, adminMap)

    generateUncurried2 :: [String] -> JSNode -> ModuleElement -> ([ModuleElement],FuncAdminMap)
    generateUncurried2 argList block (Member jsNode typ name1 [decl2] keys)
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl2
        , JSExpression (e : op : _decl3) <- node jsNode
        =   let newName = name1 ++ suffix
            in case setAccessor (node e) newName of
                Nothing -> (m : eles, adminMap)
                Just newE ->
                    let newArgList    = intersperse (nt (JSLiteral ","))
                                            $ map (nt . JSIdentifier)
                                                argList
                        newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
                        newNode       = NN $ JSExpression (NN newE : op : [newDecl])
                        newAdmin      = FuncAdmin { moduleId = mid, arity = length argList, exported = False}
                    in (m : Member newNode typ newName [newDecl] keys : eles, addAdmin name1 newAdmin adminMap)
    generateUncurried2 _ _ _ = (m : eles, adminMap)
generateUncurriedEle _mid m (eles, adminMap) = (m : eles, adminMap)

analyzeUncurriedPrim :: [String] -> JSNode -> Maybe ([String], JSNode)
analyzeUncurriedPrim idList decl
    | JSBlock _ [ef] _      <- node decl
    , JSReturn _ [ef2] _    <- node ef
    , JSExpression [ef3]    <- node ef2
    , JSFunctionExpression fn _names _lb [parameter] _rb block <- node ef3
    , JSLiteral "function"  <- node fn
    , JSIdentifier idi      <- node parameter
    = trace ("found deeper: " ++ show idList ++ " para: " ++ show idi) $ analyzeUncurriedPrim (idi : idList) block
analyzeUncurriedPrim l@(_a:_b:_) block = Just (reverse l,block)
analyzeUncurriedPrim _ _ = Nothing

-- * Exports

-- | Generation of uncurried exports
generateUncurriedExports :: Module -> ([Module],FuncAdminMap) -> ([Module],FuncAdminMap)
generateUncurriedExports (Module moduleIdentifier moduleElements) (modules, adminMap) =
        let (eles,adminMap') = foldr (generateUncurriedExpo moduleIdentifier) ([],adminMap) moduleElements
        in (Module moduleIdentifier eles : modules, adminMap')

generateUncurriedExpo :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncAdminMap) -> ([ModuleElement],FuncAdminMap)
generateUncurriedExpo mid (ExportsList l) (eles, adminMap) =
    let (newExports,adminMap') = foldr (generateUncurriedEx mid) ([],adminMap) l
    in (ExportsList newExports : eles, adminMap')
generateUncurriedExpo _mid other (eles, adminMap) = (other : eles, adminMap)

generateUncurriedEx :: ModuleIdentifier -> (ExportType, String, JSNode, [Key])
    -> ([(ExportType, String, JSNode, [Key])],FuncAdminMap) -> ([(ExportType, String, JSNode, [Key])],FuncAdminMap)
generateUncurriedEx mid t@(RegularExport name1, name2, jSNode, [key]) (eles, adminMap)
    | JSIdentifier _name3 <- node jSNode
    = -- [(ExportType, String, JSNode, [Key])]
        case findAdminFor name2 mid adminMap of
            Nothing -> (t : eles, adminMap)
            Just admin ->
                let newName   = name2 ++ suffix
                    newAdmin  = admin {exported = True}
                    newNode   = nt (JSIdentifier newName)
                    newKeys   = [(fst key,newName)]
                    newExport = (RegularExport (name1 ++ suffix    ), newName, newNode, newKeys)
                in  (t : newExport : eles, replaceAdmin name2 admin newAdmin adminMap)

generateUncurriedEx mid t@(ForeignReexport, name, jSNode, [key]) (eles, adminMap)
    | JSMemberDot [l] m r <- node jSNode
    , JSIdentifier "$foreign" <- node l
    , JSLiteral "." <- node m
    , JSIdentifier name2 <- node r
    =     case findAdminFor name2 mid adminMap of
            Nothing -> (t : eles, adminMap)
            Just admin ->
                let newName   = name ++ suffix
                    newAdmin  = admin {exported = True}
                    newNode   = NN (JSMemberDot [nt (JSIdentifier "$foreign")] (nt (JSLiteral "."))
                                        (nt (JSIdentifier newName)))
                    newKeys   = [(fst key,newName)]
                    newExport = (ForeignReexport, newName, newNode, newKeys)
                in  (t : newExport : eles, replaceAdmin name2 admin newAdmin adminMap)

generateUncurriedEx _mid t (eles, adminMap) =
    trace ("export in unknown form: " ++ show t) (t : eles, adminMap)

-- * Call replacement

-- | replace satured calls to calls to uncurried functions
generateSaturedCalls :: Module -> ([Module],FuncAdminMap) -> ([Module],FuncAdminMap)
generateSaturedCalls (Module moduleIdentifier moduleElements) (modules, adminMap) =
    trace ("generateSaturedCalls: " ++ show moduleIdentifier) $
    let (eles,adminMap') = foldr (generateSaturedC moduleIdentifier) ([],adminMap) moduleElements
    in (Module moduleIdentifier eles : modules, adminMap')

-- |  Generate uncurried functions from curried functions
generateSaturedC :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncAdminMap) -> ([ModuleElement],FuncAdminMap)
generateSaturedC mid (Member jSNode' sort name decls keys) (eles, adminMap) =
    let replaceSaturedNode = rscJS jSNode'
        replaceSaturedDecls = map rscJS decls
    in  (Member replaceSaturedNode sort name replaceSaturedDecls keys : eles, adminMap)
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
        ,  JSArguments _al [am] _ar <- node n2
        ,  JSCallExpression _str _cl [cm] _cr <- node n3
        ,  JSArguments _a2l [a2m] _a2r <- node cm
        = trace ("replace candidate: " ++ name' ++ " " ++ intercalate " " (map showStripped nodes)) $
            case findAdminFor name' mid adminMap of
                Nothing -> trace ("replace candidate: not in map: " ++ name') $
                                (rscJS n1) : mapReplace (n2:n3:rest)
                Just admin ->
                    let moreCallArgs = getCallArgs rest
                        newName = name' ++ suffix
                    in if arity admin == 2 + length moreCallArgs
                        then trace ("replace!!! " ++ show (arity admin)) $
                            let newNodes = generateNewCall (JSIdentifier newName)
                                                (node n2) (node am: node a2m : moreCallArgs)
                            in mapReplace newNodes ++ drop (1 + arity admin) nodes
                        else trace ("no replace!!! " ++ show (arity admin) ++ " " ++ show (2 + length moreCallArgs)) $
                            (rscJS n1) : mapReplace (n2:n3:rest)

    mapReplace nodes@(n1:n2:n3:rest)
        |  JSMemberDot [l] m r <- node n1
        ,  JSIdentifier "$foreign" <- node l
        ,  JSIdentifier name' <- node r
        ,  JSArguments _al [am] _ar <- node n2
        ,  JSCallExpression _str _cl [cm] _cr <- node n3
        ,  JSArguments _a2l [a2m] _a2r <- node cm
        = trace ("replace candidate: " ++ name' ++ " " ++  intercalate " " (map showStripped nodes)) $
            case findAdminFor name' mid adminMap of
                Nothing -> trace ("replace candidate: not in map: " ++ name') $
                                (rscJS n1) : mapReplace (n2:n3:rest)
                Just admin ->
                    let moreCallArgs = getCallArgs rest
                        newName = name' ++ suffix
                    in if arity admin == 2 + length moreCallArgs
                        then trace ("replace!!! " ++ show (arity admin)) $
                            let newNodes = generateNewCall (JSMemberDot [l] m (nt (JSIdentifier newName)))
                                                (node n2) (node am: node a2m : moreCallArgs)
                            in mapReplace newNodes ++ drop (1 + arity admin) nodes
                        else trace ("no replace!!! " ++ show (arity admin) ++ " " ++ show (2 + length moreCallArgs)) $
                            (rscJS n1) : mapReplace (n2:n3:rest)

    mapReplace (hd:tl) = (rscJS hd) : mapReplace tl

generateSaturedC _ e (eles, adminMap) = (e : eles, adminMap)

getCallArgs :: [JSNode] -> [Node]
getCallArgs (hd:tl)
    |  JSCallExpression _str _cl [cm] _cr <- node hd
    ,  JSArguments _a2l [a2m] _a2r <- node cm
    ,  oneArg <- node a2m
    = oneArg : getCallArgs tl
getCallArgs _ = []

generateNewCall :: Node -> Node -> [Node] -> [JSNode]
generateNewCall idNode (JSArguments al [_am] ar) argNodes
    = let argList = intersperse (nt (JSLiteral ", ")) $ map nt argNodes
      in [nt idNode, nt (JSArguments al argList ar)]
generateNewCall _idNode _args _argNodes
    = error "BundleOpt>>generateNewCall: Impossible match error."
