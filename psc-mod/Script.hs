module Script where

import Control.Arrow (first)
import Data.Tuple (swap)
import Text.Parsec
import Language.PureScript.Parser
import qualified Language.PureScript as P
import qualified Data.Map as M
import System.FilePath (pathSeparator)

{-

  TODO (Type under cursor):

    - Figure out how to typecheck and expression

    - Lex the file
    - Get the position closest to the requested
    - Parse expression
    - Type check
    - Return type && $$$

-}

-- "Interactive"
(Right lexedAdd) = P.lex "" "1+1"
(Right parsedAdd) = P.runTokenParser "" (psciExpression <* eof) lexedAdd
module' = createTemporaryModule True parsedAdd
-- MIssing prelude
made = runMake $ make [(Left P.RebuildAlways, supportModule), (Left P.RebuildAlways, module')]


-- Declarations
psciExpression :: P.TokenParser P.Expr
psciExpression = P.parseValue

type ImportedModule = (P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName)

createTemporaryModule :: Bool -> P.Expr -> P.Module
createTemporaryModule exec val =
  let
    imports = []
    lets = []
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    trace = P.Var (P.Qualified (Just supportModuleName) (P.Ident "eval"))
    mainValue = P.App trace (P.Var (P.Qualified Nothing (P.Ident "it")))
    itDecl = P.ValueDeclaration (P.Ident "it") P.Public [] $ Right val
    mainDecl = P.ValueDeclaration (P.Ident "main") P.Public [] $ Right mainValue
    decls = if exec then [itDecl, mainDecl] else [itDecl]
  in
    P.Module [] moduleName ((importDecl `map` imports) ++ lets ++ decls) Nothing

importDecl :: ImportedModule -> P.Declaration
importDecl (mn, declType, asQ) = P.ImportDeclaration mn declType asQ

supportModuleName :: P.ModuleName
supportModuleName = P.ModuleName [P.ProperName "$PSCI", P.ProperName "Support"]

make :: [(Either P.RebuildPolicy FilePath, P.Module)] -> P.Make P.Environment
make ms = P.make actions ms
  where
    filePathMap = M.fromList $ (first P.getModuleName . swap) `map` ms
    actions = P.buildMakeActions modulesDir filePathMap (M.fromList []) False

modulesDir :: FilePath
modulesDir = ".psci_modules" ++ pathSeparator : "node_modules"

runMake :: P.Make a -> IO (Either P.MultipleErrors a)
runMake mk = fmap (fmap fst) $ P.runMake (P.Options False False Nothing False False False Nothing) mk

data PSCiOptions = PSCiOptions
  { psciMultiLineMode     :: Bool
  , psciInputFile         :: [FilePath]
  , psciForeignInputFiles :: [FilePath]
  , psciInputNodeFlags    :: [String]
  }

-- | Support module, contains code to evaluate terms
supportModule :: P.Module
supportModule =
  case P.parseModulesFromFiles id [("", code)] of
    Right [(_, P.Module cs _ ds exps)] -> P.Module cs supportModuleName ds exps
    _ -> error "Support module could not be parsed"
  where
  code :: String
  code = unlines
    [ "module S where"
    , ""
    , "import Prelude"
    , "import Control.Monad.Eff"
    , "import Control.Monad.Eff.Console"
    , "import Control.Monad.Eff.Unsafe"
    , ""
    , "class Eval a where"
    , "  eval :: a -> Eff (console :: CONSOLE) Unit"
    , ""
    , "instance evalShow :: (Show a) => Eval a where"
    , "  eval = print"
    , ""
    , "instance evalEff :: (Eval a) => Eval (Eff eff a) where"
    , "  eval x = unsafeInterleaveEff x >>= eval"
    ]
