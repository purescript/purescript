module Language.PureScript.Interface.Types where

import Protolude

import Control.Monad.Supply (evalSupplyT)
import Control.Monad.Trans.Writer (WriterT(..))
import Language.PureScript.AST.Declarations (importPrim, getModuleDeclarations, Expr (..), Declaration, pattern ValueDecl, pattern MkUnguarded)
import Language.PureScript.AST.SourcePos (SourceSpan (..), containsSpan, leftOf, rightOf)
import Language.PureScript.AST.Traversals (everythingOnValues)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST (parseFromFile)
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Sugar (desugar)
import Language.PureScript.Sugar.Names.Env (primEnv)
import Language.PureScript.TypeChecker (typeCheckModule, emptyCheckState, debugType)
import Text.Pretty.Simple (pPrint)
import Data.String (String)

data NodeContext = DeclarationNode | ExpressionNode
  deriving (Show)

data NodeInfo a = NodeInfo
  { nodeType :: a
  , nodeContext :: NodeContext
  } deriving (Show)

data InterfaceAST a = InterfaceNode
  { nodeInfo :: NodeInfo a
  , nodeSpan :: SourceSpan,
    nodeLeaves :: [InterfaceAST a]
  } deriving (Show)

makeNode :: NodeInfo a -> SourceSpan -> InterfaceAST a
makeNode i s = InterfaceNode i s []

combineAST :: forall a. InterfaceAST a -> InterfaceAST a -> InterfaceAST a
combineAST x@(InterfaceNode xInfo xSpan xLeaves) y@(InterfaceNode _ ySpan yLeaves)
  -- for same spans, just merge the leaves
  | xSpan == ySpan = InterfaceNode xInfo xSpan (mergeASTs xLeaves yLeaves)
  -- if x contains y, then flip the arguments
  | xSpan `containsSpan` ySpan = combineAST y x
  -- at this point, we can just insert y into x
combineAST y (InterfaceNode xInfo xSpan xLeaves) = InterfaceNode xInfo xSpan (insertAST y xLeaves)

insertAST :: forall a. InterfaceAST a -> [InterfaceAST a] -> [InterfaceAST a]
insertAST x = mergeASTs [x]

mergeASTs :: forall a. [InterfaceAST a] -> [InterfaceAST a] -> [InterfaceAST a]
mergeASTs xs [] = xs
mergeASTs [] ys = ys
mergeASTs xs@(x : xr) ys@(y : yr)
  -- x contains y, insert y to x
  | xSpan `containsSpan` ySpan = mergeASTs (combineAST x y : xr) yr
  -- y contains x, insert x to y
  | ySpan `containsSpan` xSpan = mergeASTs xr (combineAST x y : yr)
  -- x comes after y
  | xSpan `rightOf` ySpan = y : mergeASTs xs yr
  -- y comes after x
  | xSpan `leftOf` ySpan = x : mergeASTs xr ys
  -- fully disjoint, just recurse
  | otherwise = x : mergeASTs xr ys
  where
  xSpan :: SourceSpan
  xSpan = nodeSpan x

  ySpan :: SourceSpan
  ySpan = nodeSpan y

source :: Text
source = unlines
  [ "module Main where"
  , "values :: Array Int"
  , "values = [0, 1]"
  ]

main :: IO ()
main = do
  case snd $ parseFromFile "<stdin>" source of
    Left _ -> internalError "Could not parse file."
    Right parsedModule -> do
      checkResult <- runExceptT $ fmap fst $ runWriterT $ evalSupplyT 0 $ do
        (desugardModule, (externsEnv, _)) <- runStateT (desugar [] (importPrim parsedModule)) (primEnv, mempty)
        let moduleExports = (\(_, _, exports) -> exports) <$> externsEnv
        evalStateT (typeCheckModule moduleExports desugardModule) (emptyCheckState initEnvironment)
      case checkResult of
        Left _ -> internalError "Could not check file."
        Right checkedModule' ->
          forM_ (concatMap declarationToAST $ getModuleDeclarations checkedModule') $ \i ->
            pPrint i

  where
  declarationToAST :: Declaration -> [InterfaceAST String]
  (declarationToAST, _, _, _, _) = everythingOnValues mergeASTs onDecl onExpr noAST noAST noAST 
    where
    noAST :: forall a. a -> [InterfaceAST String]
    noAST = const []

    onDecl :: Declaration -> [InterfaceAST String]
    onDecl = \case
      ValueDecl (s, _) _ _ _ [MkUnguarded (TypedValue _ (PositionedValue _ _ _) t)] ->
        [ makeNode (NodeInfo (debugType t) DeclarationNode) s ]
      _ ->
        []

    onExpr :: Expr -> [InterfaceAST String]
    onExpr = \case
      TypedValue _ (PositionedValue s _ _) t ->
        [ makeNode (NodeInfo (debugType t) ExpressionNode) s ]
      _ ->
        []
