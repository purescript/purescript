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

import Language.PureScript.BundleTypes
import Language.JavaScript.Parser.AST

-- | Main function for uncurry optimization
uncurryFunc :: [Module] -> [Module]
uncurryFunc modules =
    -- add uncurried functions
    let (modulesWithUncurried,adminMap) = foldr generateUncurried ([],M.empty) modules
    -- add exports for uncurried functions
        (modulesWithExports,adminMap2)  = foldr generateUncurriedExports ([],adminMap) modulesWithUncurried
    -- replace satured calls to calls to uncurried functions
        (modulesWithCalls,adminMap3)    = foldr generateSaturedCalls ([],adminMap2) modulesWithExports

    in  trace (show adminMap2) $ modulesWithCalls

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
        replaceFunc l = (filter (\e -> e /= oldFuncAdmin) l) ++  [newFuncAdmin]
        replaceFunc _ = error "BundleOpt>>replaceAdmin: Impossible with FuncAdmin"

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
generateUncurriedEle mid m@(Member jSNode sort name [decl] keys) (eles, adminMap) -- a var decl
    | JSFunctionExpression fn _names lb [parameter] rb block <- node decl
    , JSLiteral "function" <- node fn
    , JSIdentifier idi <- node parameter
        = trace ("candidate: " ++ name ++ " para: " ++ show idi) $
            case analyzeUncurriedPrim [idi] block of
                Nothing -> (m : eles, adminMap)
                Just (argList,block) -> if not sort
                                            then trace ("generateFor: " ++ name) $ generateUncurried1 argList block m
                                            else trace ("generateFor2: " ++ name) $ generateUncurried2 argList block m
  where
    generateUncurried1 :: [String] -> JSNode -> ModuleElement -> ([ModuleElement],FuncAdminMap)
    generateUncurried1 argList block m@(Member jsNode typ name [decl] keys)
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl
        , JSVariables var [ varIntro ] rb2 <- node jsNode
        , JSVarDecl declN (eq : decl) <- node varIntro
        = let newName = name ++ "_"
              newArgList    = intersperse (nt (JSLiteral ","))
                                    $ map (nt . JSIdentifier)
                                        argList
              newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
              newVarIntro   = NN $ JSVarDecl (sp $ JSIdentifier newName) (eq : [newDecl])
              newNode       = NN $ JSVariables var [newVarIntro] rb2
              newAdmin      = FuncAdmin { moduleId = mid, arity = length argList, exported = False}
              in (m : Member newNode typ newName [newDecl] keys : eles, addAdmin name newAdmin adminMap)
    generateUncurried1 _ _ _ = (m : eles, adminMap)

    generateUncurried2 :: [String] -> JSNode -> ModuleElement -> ([ModuleElement],FuncAdminMap)
    generateUncurried2 argList block m@(Member jsNode typ name1 [decl] keys)
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl
        , JSExpression (e : op : decl) <- node jsNode
        =   let newName = name ++ "_"
            in case setAccessor (node e) newName of
                Nothing -> (m : eles, adminMap)
                Just newE ->
                    let newArgList    = intersperse (nt (JSLiteral ","))
                                            $ map (nt . JSIdentifier)
                                                argList
                        newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
                        newNode       = NN $ JSExpression (NN newE : op : [newDecl])
                        newAdmin      = FuncAdmin { moduleId = mid, arity = length argList, exported = False}
                    in (m : Member newNode typ newName [newDecl] keys : eles, addAdmin name newAdmin adminMap)
    generateUncurried2 _ _ _ = (m : eles, adminMap)
generateUncurriedEle _mid m (eles, adminMap) = (m : eles, adminMap)

analyzeUncurriedPrim :: [String] -> JSNode -> Maybe ([String], JSNode)
analyzeUncurriedPrim idList decl
    | JSBlock _ [ef] _      <- node decl
    , JSReturn _ [ef2] _    <- node ef
    , JSExpression [ef3]    <- node ef2
    , JSFunctionExpression fn _names lb [parameter] rb block <- node ef3
    , JSLiteral "function"  <- node fn
    , JSIdentifier idi      <- node parameter
    = trace ("found deeper: " ++ show idList ++ " para: " ++ show idi) $ analyzeUncurriedPrim (idi : idList) block
analyzeUncurriedPrim l@(a:b:_) block = Just (reverse l,block)
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
generateUncurriedExpo mid other (eles, adminMap) = (other : eles, adminMap)

generateUncurriedEx :: ModuleIdentifier -> (ExportType, String, JSNode, [Key])
    -> ([(ExportType, String, JSNode, [Key])],FuncAdminMap) -> ([(ExportType, String, JSNode, [Key])],FuncAdminMap)
generateUncurriedEx mid t@(RegularExport name1, name2, jSNode, [key]) (eles, adminMap)
    | JSIdentifier name3 <- node jSNode
    = -- [(ExportType, String, JSNode, [Key])]
        case findAdminFor name2 mid adminMap of
            Nothing -> (t : eles, adminMap)
            Just admin ->
                let newName   = name2 ++ "_"
                    newAdmin  = admin {exported = True}
                    newNode   = nt (JSIdentifier newName)
                    newKeys   = [(fst key,newName)]
                    newExport = (RegularExport (name1 ++ "_"), newName, newNode, newKeys)
                in  (t : newExport : eles, replaceAdmin name2 admin newAdmin adminMap)

generateUncurriedEx mid t@(ForeignReexport, name, jSNode, [key]) (eles, adminMap)
    | JSMemberDot [l] m r <- node jSNode
    , JSIdentifier "$foreign" <- node l
    , JSLiteral "." <- node m
    , JSIdentifier name2 <- node r
    =     case findAdminFor name2 mid adminMap of
            Nothing -> (t : eles, adminMap)
            Just admin ->
                let newName   = name ++ "_"
                    newAdmin  = admin {exported = True}
                    newNode   = NN (JSMemberDot [nt (JSIdentifier "$foreign")] (nt (JSLiteral "."))
                                        (nt (JSIdentifier newName)))
                    newKeys   = [(fst key,newName)]
                    newExport = (ForeignReexport, newName, newNode, newKeys)
                in  (t : newExport : eles, replaceAdmin name2 admin newAdmin adminMap)

generateUncurriedEx mid t (eles, adminMap) =
    trace ("export in unknown form: " ++ show t) (t : eles, adminMap)

-- * Call replacement

-- | replace satured calls to calls to uncurried functions
generateSaturedCalls :: Module -> ([Module],FuncAdminMap) -> ([Module],FuncAdminMap)
generateSaturedCalls (Module moduleIdentifier moduleElements) (modules, adminMap) =
    let (eles,adminMap') = foldr (generateSaturedC moduleIdentifier) ([],adminMap) moduleElements
    in (Module moduleIdentifier eles : modules, adminMap')

-- |  Generate uncurried functions from curried functions
generateSaturedC :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncAdminMap) -> ([ModuleElement],FuncAdminMap)
generateSaturedC mid m@(Member jSNode sort name [decl] keys) (eles, adminMap) =
    let replaceSaturedNode = rscJS adminMap jSNode
        replaceSaturedDecl = rscJS adminMap decl
    in  (Member replaceSaturedNode sort name [replaceSaturedDecl] keys : eles, adminMap)
  where
    rscJS :: FuncAdminMap -> JSNode -> JSNode
    rscJS adminMap (NN node) = (NN (rsc adminMap node))
    rscJS adminMap (NT node tokenPosn commentAnnotations) = (NT (rsc adminMap node) tokenPosn commentAnnotations)

    -- | AST traversal to replace satured function calls
    rsc :: FuncAdminMap -> Node -> Node
    rsc adminMap (JSArguments jSNodeL jSNodesM jSNodeR) =
        JSArguments (rscJS adminMap jSNodeL) (mapReplace adminMap jSNodesM) (rscJS adminMap jSNodeR)
    rsc adminMap (JSArrayLiteral jSNodeL jSNodesM jSNodeR)	=
        JSArrayLiteral (rscJS adminMap jSNodeL) (mapReplace adminMap jSNodesM) (rscJS adminMap jSNodeR)
    rsc adminMap (JSBlock jSNodesL jSNodesM jSNodesR) =
    	JSBlock (mapReplace adminMap jSNodesL) (mapReplace adminMap jSNodesM) (mapReplace adminMap jSNodesR)
    rsc adminMap (JSBreak jSNodeL jSNodesM jSNodeR) =
        JSBreak (rscJS adminMap jSNodeL) (mapReplace adminMap jSNodesM) (rscJS adminMap jSNodeR)
    rsc adminMap (JSCallExpression string jSNodesL jSNodesM jSNodesR) =
     	JSCallExpression string (mapReplace adminMap jSNodesL) (mapReplace adminMap jSNodesM) (mapReplace adminMap jSNodesR)
    rsc adminMap (JSCase jSNodeL jSNodeM jSNodeR jSNodesE) =
        JSCase (rscJS adminMap jSNodeL) (rscJS adminMap jSNodeM) (rscJS adminMap jSNodeR) (mapReplace adminMap jSNodesE)
    rsc adminMap (JSCatch jSNodeL jSNodeM jSNodeR jSNodesE1 jSNodeE2 jSNodeE3) =
        JSCatch (rscJS adminMap jSNodeL) (rscJS adminMap jSNodeM) (rscJS adminMap jSNodeR) (mapReplace adminMap jSNodesE1)
                (rscJS adminMap jSNodeE2) (rscJS adminMap jSNodeE3)
    rsc adminMap (JSContinue jSNodeL jSNodesM jSNodeR) =
     	JSContinue (rscJS adminMap jSNodeL) (mapReplace adminMap jSNodesM) (rscJS adminMap jSNodeR)
    rsc adminMap (JSDefault jSNodeL jSNodeM jSNodesR) =
    	JSDefault (rscJS adminMap jSNodeL) (rscJS adminMap jSNodeM) (mapReplace adminMap jSNodesR)
    rsc adminMap (JSDoWhile jSNode1 jSNode2 jSNode3 jSNode4 jSNode5 jSNode6 jSNode7) =
        JSDoWhile (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3) (rscJS adminMap jSNode4)
                    (rscJS adminMap jSNode5) (rscJS adminMap jSNode6) (rscJS adminMap jSNode7)
    rsc adminMap (JSElision jSNode) =
    	JSElision (rscJS adminMap jSNode)
    rsc adminMap (JSExpression jSNodes) =
    	JSExpression (mapReplace adminMap jSNodes)
    rsc adminMap (JSExpressionBinary string jSNodesL jSNodeM jSNodesR) =
        JSExpressionBinary string (mapReplace adminMap jSNodesL) (rscJS adminMap jSNodeM) (mapReplace adminMap jSNodesR)
    rsc adminMap (JSExpressionParen jSNodeL jSNodeM jSNodeR) =
        JSExpressionParen (rscJS adminMap jSNodeL) (rscJS adminMap jSNodeM) (rscJS adminMap jSNodeR)
    rsc adminMap (JSExpressionPostfix string jSNodesL jSNodeR) =
        JSExpressionPostfix string (mapReplace adminMap jSNodesL) (rscJS adminMap jSNodeR)
    rsc adminMap (JSExpressionTernary jSNodesL jSNodeM jSNodesR jSNodeE1 jSNodesE2) =
        JSExpressionTernary (mapReplace adminMap jSNodesL) (rscJS adminMap jSNodeM)(mapReplace adminMap jSNodesR)
            (rscJS adminMap jSNodeE1)(mapReplace adminMap jSNodesE2)
    rsc adminMap (JSFinally jSNodeL jSNodeR) =
        JSFinally (rscJS adminMap jSNodeL) (rscJS adminMap jSNodeR)
    rsc adminMap (JSFor jSNode1 jSNode2 jSNodes3 jSNode4 jSNodes5 jSNode6 jSNodes7 jSNode8 jSNode9) =
        JSFor (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (mapReplace adminMap jSNodes3)
                (rscJS adminMap jSNode4) (mapReplace adminMap jSNodes5)
                (rscJS adminMap jSNode6) (mapReplace adminMap jSNodes7)
                (rscJS adminMap jSNode8) (rscJS adminMap jSNode9)
    rsc adminMap (JSForIn jSNode1 jSNode2 jSNodes3 jSNode4 jSNode5 jSNode6 jSNode7) =
        JSForIn (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (mapReplace adminMap jSNodes3)
            (rscJS adminMap jSNode4) (rscJS adminMap jSNode5)
            (rscJS adminMap jSNode6) (rscJS adminMap jSNode7)
    rsc adminMap (JSForVar jSNode1 jSNode2 jSNode3 jSNodes4 jSNode5 jSNodes6 jSNode7 jSNodes8 jSNode9 jSNode10) =
        JSForVar (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
                (mapReplace adminMap jSNodes4) (rscJS adminMap jSNode5)
                (mapReplace adminMap jSNodes6) (rscJS adminMap jSNode7)
                (mapReplace adminMap jSNodes8) (rscJS adminMap jSNode9) (rscJS adminMap jSNode10)
    rsc adminMap (JSForVarIn jSNode1 jSNode2 jSNode3 jSNode4 jSNode5 jSNode6 jSNode7 jSNode8) =
        JSForVarIn (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
            (rscJS adminMap jSNode4) (rscJS adminMap jSNode5)
            (rscJS adminMap jSNode6) (rscJS adminMap jSNode6)
            (rscJS adminMap jSNode8)
    rsc adminMap (JSFunction jSNode1 jSNode2 jSNode3 jSNodes4 jSNode5 jSNode6) =
        JSFunction (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
            (mapReplace adminMap jSNodes4) (rscJS adminMap jSNode5)
            (rscJS adminMap jSNode6)
    rsc adminMap (JSFunctionExpression jSNode1 jSNodes2 jSNode3 jSNodes4 jSNode5 jSNode6) =
        JSFunctionExpression (rscJS adminMap jSNode1) (mapReplace adminMap jSNodes2) (rscJS adminMap jSNode3)
            (mapReplace adminMap jSNodes4) (rscJS adminMap jSNode5)
            (rscJS adminMap jSNode6)
    rsc adminMap (JSIf jSNode1 jSNode2 jSNode3 jSNode4 jSNodes5 jSNodes6) =
        JSIf (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
            (rscJS adminMap jSNode4) (mapReplace adminMap jSNodes5)
            (mapReplace adminMap jSNodes6)
    rsc adminMap (JSLabelled jSNode1 jSNode2 jSNode3) =
        JSLabelled (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
    rsc adminMap (JSMemberDot jSNodes1 jSNode2 jSNode3) =
        JSMemberDot (mapReplace adminMap jSNodes1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
    rsc adminMap (JSMemberSquare jSNodes1 jSNode2 jSNode3 jSNode4) =
        JSMemberSquare (mapReplace adminMap jSNodes1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
                (rscJS adminMap jSNode4)
    rsc adminMap (JSObjectLiteral jSNode1 jSNodes2 jSNode3) =
        JSObjectLiteral (rscJS adminMap jSNode1) (mapReplace adminMap jSNodes2) (rscJS adminMap jSNode3)
    rsc adminMap (JSOperator jSNode) =
        JSOperator (rscJS adminMap jSNode)
    rsc adminMap (JSPropertyAccessor jSNode1 jSNode2 jSNode3 jSNodes4 jSNode5 jSNode6) =
        JSPropertyAccessor (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
            (mapReplace adminMap jSNodes4) (rscJS adminMap jSNode5)
            (rscJS adminMap jSNode6)
    rsc adminMap (JSPropertyNameandValue jSNode1 jSNode2 jSNodes3) =
        JSPropertyNameandValue (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (mapReplace adminMap jSNodes3)
    rsc adminMap (JSReturn jSNode1 jSNodes2 jSNode3) =
        JSReturn (rscJS adminMap jSNode1) (mapReplace adminMap jSNodes2) (rscJS adminMap jSNode3)
    rsc adminMap (JSSourceElementsTop jSNodes) =
        JSSourceElementsTop (mapReplace adminMap jSNodes)
    rsc adminMap (JSSwitch jSNode1 jSNode2 jSNode3 jSNode4 jSNode5) =
        JSSwitch (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
            (rscJS adminMap jSNode4) (rscJS adminMap jSNode5)
    rsc adminMap (JSThrow jSNode1 jSNode2) =
        JSThrow (rscJS adminMap jSNode1) (rscJS adminMap jSNode2)
    rsc adminMap (JSTry jSNode1 jSNode2 jSNodes3) =
        JSTry (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (mapReplace adminMap jSNodes3)
    rsc adminMap (JSUnary string jSNode) =
        JSUnary string (rscJS adminMap jSNode)
    rsc adminMap (JSVarDecl jSNode jSNodes) =
        JSVarDecl (rscJS adminMap jSNode) (mapReplace adminMap jSNodes)
    rsc adminMap (JSVariables jSNode1 jSNodes2 jSNode3) =
        JSVariables (rscJS adminMap jSNode1) (mapReplace adminMap jSNodes2) (rscJS adminMap jSNode3)
    rsc adminMap (JSWhile jSNode1 jSNode2 jSNode3 jSNode4 jSNode5) =
        JSWhile (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
            (rscJS adminMap jSNode4) (rscJS adminMap jSNode5)
    rsc adminMap (JSWith jSNode1 jSNode2 jSNode3 jSNode4 jSNodes5) =
        JSWith (rscJS adminMap jSNode1) (rscJS adminMap jSNode2) (rscJS adminMap jSNode3)
            (rscJS adminMap jSNode4) (mapReplace adminMap jSNodes5)
    rsc _ e = e

    mapReplace :: FuncAdminMap -> [JSNode] -> [JSNode]
    mapReplace funcAdminMap nodes@(n1:n2:n3:rest)
        |  JSIdentifier name <- node n1
        ,  JSArguments al [am] ar <- node n2
        ,  JSCallExpression str cl [cm] cr <- node n3
        ,  JSArguments a2l [a2m] a2r <- node cm
        = trace ("replace candidate: " ++ name) $
            case findAdminFor name mid funcAdminMap of
                Nothing -> trace ("replace candidate: not in map: " ++ name) map (rscJS funcAdminMap) nodes
                Just admin ->
                    let moreCallFuncs = getCallFunc rest
                    in if arity admin == 2 + length moreCallFuncs
                        then trace "replace!!!" $ map (rscJS funcAdminMap) nodes
                        else map (rscJS funcAdminMap) nodes

    mapReplace funcAdminMap nodes = map (rscJS funcAdminMap) nodes

generateSaturedC _ e (eles, adminMap) = (e : eles, adminMap)


getCallFunc (hd:tl)
    |  JSCallExpression str cl [cm] cr <- node hd
    ,  JSArguments a2l [a2m] a2r <- node cm
    ,  oneArg <- node a2m
    = oneArg : getCallFunc tl
getCallFunc _ = []
